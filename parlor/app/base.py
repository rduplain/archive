import abc
import sys
import uuid
from urllib.parse import urlparse, urlunparse, parse_qsl

import jeni
import werkzeug.exceptions
from werkzeug.routing import Map as UrlMap
from werkzeug.routing import Rule as UrlRule
from werkzeug.routing import Submount

from ..exception import ApplicationException
from ..exception import Redirect, NotFound, MethodNotAllowed
from ..plan import Plan
from ..provider import dict_provider, multi_dict_provider


class Injector(jeni.Injector):
    "parlor namespace for registered providers"


from ..log import logger_factory


Injector.factory('logger', logger_factory)
Injector.value('uid_factory', lambda: str(uuid.uuid4()))


class Application(object, metaclass=abc.ABCMeta):
    METHODS = ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH']
    injector_class = Injector
    injector_plan_class = Plan

    def __init__(self, master_plan):
        class ApplicationInjector(self.injector_class):
            "Injector namespace for this Application instance."
        self.injector_class = ApplicationInjector

        self.url_map = UrlMap()
        self.endpoint_to_fn_map = {}
        self.fn_to_plan_map = {}
        self.plan_to_injector_class_map = {}

        plans = list(master_plan)

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

        ApplicationInjector.provider(
            'config', self.build_dict_provider(master_plan.config))

        self.setup(plans, self.handle_request, **master_plan.config)

    @abc.abstractmethod
    def setup(self, plans, fn, **config):
        "Register given fn to handle all routes for all methods."

    @abc.abstractmethod
    def prepare_request(self, injector_plan, *a, **kw):
        """Per request: provide data to injector, return request detail.

        Return value should be a tuple (url, method, session_uid) with url as
        full URL of request and string session_uid, set as None if not
        available.
        """

    @abc.abstractmethod
    def get_implementation(self):
        "Return concrete application implementation, e.g. the WSGI app."

    @abc.abstractmethod
    def build_response_application_exception(self, exc, session_uid=None):
        "Build a response for the given ApplicationException."

    @abc.abstractmethod
    def build_response(self, fn_result, session_uid=None):
        "Build a response, standard path. Put session_uid in response."

    def build_response_handled_error(self, handled_error, session_uid=None):
        return self.build_response(handled_error, session_uid)

    def build_response_unhandled_error(self, exc_type, exc_value, tb=None):
        self.raise_error(exc_type, exc_value, tb)

    def match(self, path, method):
        try:
            endpoint, arguments = self.url_adapter.match(path, method)
            if endpoint not in self.endpoint_to_fn_map:
                # URL routed without callable; intended to build URLs only.
                raise NotFound()
            return endpoint, self.endpoint_to_fn_map[endpoint], arguments
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
        return self.try_handler(None, self.handle_request_stage1, *a, **kw)

    def determine_session_uid(self, injector):
        if injector is None:
            return None
        try:
            return injector.get('session:uid')
        except LookupError:
            return None

    def try_handler(self, injector, fn, *a, **kw):
        try:
            return fn(*a, **kw)
        except Redirect as redirect:
            url = redirect.location
            if url.startswith('/'):
                if injector is not None:
                    try:
                        root_url = injector.get('root_url')
                    except (LookupError, jeni.UnsetError):
                        root_url = ''
                    if root_url:
                        url = root_url + url.lstrip('/')
            # else: assume the given url is already full location
            redirect.location = url
            sid = self.determine_session_uid(injector)
            return self.build_response_application_exception(redirect, sid)
        except ApplicationException as exc:
            sid = self.determine_session_uid(injector)
            return self.build_response_application_exception(exc, sid)
        except Exception:
            return self.build_response_unhandled_error(*sys.exc_info())

    def handle_request_stage1(self, *a, **kw):
        "Build an injector instance to handle the current request."
        req_plan = self.injector_plan_class()
        url, method, session_uid = self.prepare_request(req_plan, *a, **kw)
        req_plan.value('url', url)
        req_plan.value('method', method)
        req_plan.value('session_uid', session_uid)

        parsed = urlparse(url)
        root_url = urlunparse((parsed[0], parsed[1], '', '', '', ''))
        req_plan.value('root_url', root_url)

        endpoint, fn, arguments = self.match(parsed.path, method)
        req_plan.value('endpoint', endpoint)

        if parsed.query:
            args = {}
            args.update(arguments)
            args.update(dict(parse_qsl(parsed.query)))
        else:
            args = arguments
        req_plan.provider('arg', self.build_dict_provider(args))
        req_plan.provider('args',
            self.build_multi_dict_provider(args, require_key=False))

        plan = self.fn_to_plan_map[fn]
        class RequestInjector(self.plan_to_injector_class_map[plan]):
            "Injector namespace for the current request."
        injector_class = \
            self.apply_injector_registration(RequestInjector, req_plan)

        with injector_class() as i:
            rsp = self.try_handler(i, self.handle_request_stage2, i, fn, plan)
            if rsp is None:
                raise ValueError('Response is None.')
            return rsp

    def handle_request_stage2(self, injector, fn, plan):
        "Run the routed function using the injector and the plan."
        try:
            for handler in plan.iter_before_request_handlers():
                injector.apply_regardless(handler)
            result = injector.apply_regardless(plan.call, injector, fn)
            for handler in plan.iter_after_request_handlers():
                injector.apply_regardless(handler, result)
        except Exception as error:
            try:
                handled = self.handle_error(injector, plan, error)
                sid = self.determine_session_uid(injector)
                return self.build_response_handled_error(handled, sid)
            except Exception:
                # Error may have been re-raised, but not necessarily.
                return self.build_response_unhandled_error(*sys.exc_info())
        else:
            sid = self.determine_session_uid(injector)
            return self.build_response(result, sid)

    def handle_error(self, injector, plan, error):
        exc_type, exc_value, tb = sys.exc_info()
        assert exc_value is error
        for error_type, handler in plan.iter_errorhandlers():
            if isinstance(error, error_type):
                return injector.apply_regardless(handler, error)
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
        return dict_provider(dict_, require_key=require_key)

    def build_multi_dict_provider(self, multi_dict, require_key=True):
        return multi_dict_provider(multi_dict, require_key=require_key)
