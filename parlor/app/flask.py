import sys

import flask

from parlor import Application
from parlor.exception import ApplicationException
from parlor.exception import Redirect, NotFound, MethodNotAllowed


class FlaskApplication(Application):
    def setup(self, plans, fn, **config):
        self.app = flask.Flask(
            __name__,
            static_folder=None,
            template_folder=None)
        self.app.config.update(config)
        self.app.add_url_rule('/', 'root', fn, methods=self.METHODS)
        self.app.add_url_rule('/<path:path>', 'path', fn, methods=self.METHODS)

    def prepare_request(self, injector_plan, *a, **kw):
        request = flask.request._get_current_object()
        provider = injector_plan.provider
        provider('cookie', self.build_dict_provider(request.cookies))
        provider('form', self.build_dict_provider(request.form))
        provider('forms', self.build_multi_dict_provider(request.form))
        provider('file', self.build_dict_provider(request.files))
        provider('files', self.build_multi_dict_provider(request.files))
        provider('header', self.build_dict_provider(request.headers))
        provider('headers', self.build_multi_dict_provider(request.headers))
        session_uid = flask.session.get('session_uid')
        return request.url, request.method, session_uid

    def get_implementation(self):
        return self.app

    def build_response_application_exception(self, exc, session_uid=None):
        try:
            raise exc
        except Redirect as redirect:
            r = flask.redirect(redirect.location)
        except NotFound:
            r = ('Not Found', 404, {'Content-Type': 'text/plain'})
        except MethodNotAllowed:
            r = ('Method Not Allowed', 405, {'Content-Type': 'text/plain'})
        except ApplicationException:
            return self.build_response_unhandled_error(*sys.exc_info())
        return self._build_response(r, session_uid)

    def build_response(self, fn_result, session_uid=None):
        return self._build_response(fn_result, session_uid)

    def _build_response(self, response, session_uid=None):
        if session_uid is not None:
            flask.session['session_uid'] = session_uid
        return self.make_response(response)

    def make_response(self, *a, **kw):
        return self.app.make_response(*a, **kw)

    def get_test_client(self, *a, **kw):
        return self.app.test_client(*a, **kw)
