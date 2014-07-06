import abc
import sys

import jeni
import werkzeug.exceptions
from werkzeug.routing import Map as UrlMap
from werkzeug.routing import Rule as UrlRule
from werkzeug.routing import Submount

from ..exception import ApplicationException
from ..exception import Redirect, NotFound, MethodNotAllowed
from ..plan import InjectorPlan


class Injector(jeni.Injector):
    "parlor namespace for registered providers"


from ..log import logger_factory


Injector.factory('logger', logger_factory)


class Application(object, metaclass=abc.ABCMeta):
    METHODS = ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH']
    injector_class = Injector
    injector_plan_class = InjectorPlan

    def __init__(self, plans):
        class ApplicationInjector(self.injector_class):
            "Injector namespace for this Application instance."
        self.injector_class = ApplicationInjector

        self.url_map = UrlMap()
        self.endpoint_to_fn_map = {}
        self.fn_to_plan_map = {}
        self.plan_to_injector_class_map = {}

        for plan in plans:
            rule_obj_list = []
            for rule, methods, endpoint, fn in plan.iter_routes():
                if fn is not None:
                    self.fn_to_plan_map[fn] = plan
                    self.endpoint_to_fn_map[endpoint] = fn
                rule_obj_list.append(
                    UrlRule(rule, endpoint=endpoint, methods=methods))
            if plan.has_prefix():
                self.url_map.add(Submount(plan.get_prefix(), rule_obj_list))
            else:
                for rule_obj in rule_obj_list:
                    self.url_map.add(rule_obj)
            self.plan_to_injector_class_map[plan] = \
                self.build_plan_injector(plan)

        self.url_adapter = self.url_map.bind(server_name='unused')
        ApplicationInjector.value(
            'url_for',
            lambda endpoint, **values:\
                self.url_adapter.build(endpoint, values))

        self.setup(plans, self.handle_request)

    @abc.abstractmethod
    def setup(self, plans, fn):
        "Register given fn to handle all routes for all methods."

    @abc.abstractmethod
    def prepare_request(self, injector_plan, *a, **kw):
        "Per request: provide data to injector, return root_url, path, method."

    @abc.abstractmethod
    def get_implementation(self):
        "Return concrete application implementation, e.g. the WSGI app."

    @abc.abstractmethod
    def build_response_application_exception(self, exc):
        "Build a response for the given ApplicationException."

    def build_response(self, fn_result):
        return fn_result

    def build_response_handled_error(self, handled_error):
        return handled_error

    def build_response_unhandled_error(self, exc_type, exc_value, tb=None):
        self.raise_error(exc_type, exc_value, tb)

    def match(self, path, method):
        try:
            endpoint, arguments = self.url_adapter.match(path, method)
            if endpoint not in self.endpoint_to_fn_map:
                # URL routed without callable; intended to build URLs only.
                raise NotFound()
            return self.endpoint_to_fn_map[endpoint], arguments
        except werkzeug.exceptions.NotFound:
            raise NotFound()
        except werkzeug.exceptions.MethodNotAllowed:
            raise MethodNotAllowed()
        except werkzeug.routing.RequestRedirect as request_redirect:
            unused_url = 'http://unused'
            url = request_redirect.new_url
            if url.startswith(unused_url):
                url = url[len(unused_url):] # Strip unused_url.
            raise Redirect(url)

    def handle_request(self, *a, **kw):
        try:
            req_plan = self.injector_plan_class()
            root_url, path, method = self.prepare_request(req_plan, *a, **kw)
            fn, arguments = self.match(path, method)
            req_plan.provider('arg', self.build_dict_provider(arguments))

            plan = self.fn_to_plan_map[fn]
            class RequestInjector(self.plan_to_injector_class_map[plan]):
                "Injector namespace for the current request."
            injector_class = \
                self.apply_injector_registration(RequestInjector, req_plan)

            response = self.try_handle_request(injector_class, fn, plan)
            if response is None:
                raise ValueError('Response is None.')
            return response
        except Redirect as redirect:
            url = redirect.location
            if url.startswith('/'):
                try:
                    root_url
                except NameError:
                    root_url = ''
                if root_url:
                    url = root_url + url.lstrip('/')
            # else: assume the given url is already full location
            redirect.location = url
            return self.build_response_application_exception(redirect)
        except ApplicationException as exc:
            return self.build_response_application_exception(exc)
        except Exception:
            return self.build_response_unhandled_error(*sys.exc_info())

    def try_handle_request(self, injector_class, fn, plan):
        with injector_class() as injector:
            try:
                for handler in plan.iter_before_request_handlers():
                    injector.apply_regardless(handler)
                result = injector.apply_regardless(fn)
                for handler in plan.iter_after_request_handlers():
                    injector.apply_regardless(handler, result)
            except Exception as error:
                try:
                    handled = self.handle_error(injector, plan, error)
                    return self.build_response_handled_error(handled)
                except Exception:
                    # Error may have been re-raised, but not necessarily.
                    return self.build_response_unhandled_error(*sys.exc_info())
            else:
                return self.build_response(result)

    def handle_error(self, injector, plan, error):
        exc_type, exc_value, tb = sys.exc_info()
        assert exc_value is error
        for error_type, handler in plan.iter_errorhandlers():
            if isinstance(error, error_type):
                injector.apply_regardless(handler, error)
        self.raise_error(exc_type, exc_value, tb)

    def raise_error(self, exc_type, exc_value, tb=None):
        if exc_value.__traceback__ is not tb:
            raise exc_value.with_traceback(tb)
        raise exc_value

    def apply_injector_registration(self, injector_class, plan):
        for recipe_name, note, recipe_keywords in plan.iter_provider_data():
            register = getattr(injector_class, recipe_name)
            register(note, **recipe_keywords)
        return injector_class

    def build_plan_injector(self, plan):
        class PlanInjector(self.injector_class):
            "Injector namespace for this Plan within this Application."
        PlanInjector.provider(
            'config', self.build_dict_provider(plan.config, require_key=False))
        injector_class = self.apply_injector_registration(PlanInjector, plan)
        return injector_class

    def build_dict_provider(self, dict_, require_key=True):
        class DictProvider(jeni.Provider):
            def get(self, name=None):
                if name is None:
                    if require_key:
                        raise jeni.UnsetError()
                    return dict_
                if name not in dict_:
                    raise jeni.UnsetError()
                return dict_[name]
        return DictProvider

    def build_multi_dict_provider(self, multi_dict, require_key=True):
        class MultiDictProvider(jeni.Provider):
            def get(self, name=None):
                if name is None:
                    if require_key:
                        raise jeni.UnsetError()
                    return multi_dict
                if name not in multi_dict:
                    raise jeni.UnsetError()
                return multi_dict.getlist(name)
        return MultiDictProvider
