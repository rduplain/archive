class InitError(RuntimeError):
    """Specific RuntimeError provided to users for project init errors.

    This is useful if RuntimeError is the most appropriate Exception to raise.
    Use this base class for user exceptions which occur during module import.
    """

class ApplicationException(Exception):
    "Base class for all Application exceptions."


class BadRequest(ApplicationException):
    "Request not understood."


class Unauthorized(ApplicationException):
    "Requires authentication."


class Forbidden(ApplicationException):
    "Forbidden to perform requested action."


class NotFound(ApplicationException):
    "Route is not found."


class MethodNotAllowed(ApplicationException):
    "Method is not allowed on given route."


class Redirect(ApplicationException):
    "Redirect request to another location. Put 'Location: <url>' in body."

    def __init__(self, location, *a, **kw):
        self.location = location
        msg = 'Location: {}'.format(location)
        super(Redirect, self).__init__(msg, *a, **kw)
