import copy
from threading import Lock


class Plan(object):
    def __init__(self, *plan_items, **config):
        self.plan_items = plan_items
        self._prefix = None
        self.config = {}
        if config is not None:
            self.config.update(config)
        self.routes = {}
        self._got_first_request = False
        self.before_request_fns = []
        self.before_first_request_fns = []
        self.before_first_request_lock = Lock()
        self.after_request_fns = []
        self.errorhandlers = {}
        self.suggestions = []
        self.provider_data = {}

    def __iter__(self):
        parent_plan = self.copy()
        for prefix, child_plan in self.plan_items:
            for plan_copy in child_plan:
                plan_copy.set_prefix(prefix)
                plan_copy.update(parent_plan)
                yield plan_copy
        yield parent_plan

    def update(self, plan):
        self.config.update(plan.config)
        self.routes.update(plan.routes)
        self.before_request_fns = \
            plan.before_request_fns + self.before_request_fns
        self.before_first_request_fns = \
            plan.before_first_request_fns + self.before_first_request_fns
        self.after_request_fns.extend(plan.after_request_fns)
        self.errorhandlers.update(plan.errorhandlers)
        self.suggestions.extend(plan.suggestions)
        self.provider_data.update(plan.provider_data)

    def copy(self):
        # One-level deep shallow copy of registration data.
        plan = self.__class__()
        plan.plan_items = copy.copy(self.plan_items)
        plan._prefix = copy.copy(self._prefix)
        plan.config = copy.copy(self.config)
        plan.routes = copy.copy(self.routes)
        plan.before_request_fns = copy.copy(self.before_request_fns)
        plan.before_first_request_fns = \
            copy.copy(self.before_first_request_fns)
        plan.after_request_fns = copy.copy(self.after_request_fns)
        plan.errorhandlers = copy.copy(self.errorhandlers)
        plan.suggestions = copy.copy(self.suggestions)
        plan.provider_data = copy.copy(self.provider_data)
        return plan

    def call(self, injector, fn):
        return injector.apply_regardless(fn)

    def has_prefix(self):
        return self._prefix is not None

    def get_prefix(self):
        return self._prefix

    def set_prefix(self, prefix):
        self._prefix = None
        if prefix and prefix != '/':
            self._prefix = prefix

    def determine_methods(self, methods=None, method=None):
        if method is not None:
            if methods is not None:
                msg = 'Provide `method` or `methods`, but not both: {!r} {!r}'
                raise ValueError(msg.format(methods, method))
            return [method]
        if methods is not None:
            return methods
        return ['GET']

    def add_url_rule(self, rule, endpoint=None, fn=None,
                     methods=None, method=None):
        methods = self.determine_methods(methods=methods, method=method)
        if endpoint is None:
            if fn is None:
                msg = 'Provide `endpoint` or `fn` for {!r}'
                raise ValueError(msg.format(rule))
            endpoint = fn.__name__
        self.routes[(rule, tuple(methods))] = endpoint, fn

    def suggest_add_url_rule(self, *a, **kw):
        self.suggest('add_url_rule', *a, **kw)

    def route(self, rule, **keywords):
        """Decorate callable as a route, passes keywords to `add_url_rule`."""
        def decorator(fn):
            self.add_url_rule(rule, fn=fn, **keywords)
            return fn
        return decorator

    def suggest_route(self, rule, **keywords):
        def decorator(fn):
            self.suggest('add_url_rule', rule, fn=fn, **keywords)
            return fn
        return decorator

    def iter_routes(self):
        for (rule, methods), (endpoint, fn) in self.routes.items():
            yield rule, methods, endpoint, fn

    def errorhandler(self, exception):
        def decorator(fn):
            self.errorhandlers[exception] = fn
            return fn
        return decorator

    def iter_errorhandlers(self):
        for error_type, handler in self.errorhandlers.items():
            yield error_type, handler

    def before_request(self, fn):
        self.before_request_fns.append(fn)
        return fn

    def suggest_before_request(self, fn):
        self.suggest('before_request', fn)
        return fn

    def before_first_request(self, fn):
        self.before_first_request_fns.append(fn)
        return fn

    def suggest_before_first_request(self, fn):
        self.suggest('before_first_request', fn)
        return fn

    def iter_before_request_handlers(self):
        if not self._got_first_request:
            with self.before_first_request_lock:
                if not self._got_first_request:
                    self._got_first_request = True
                    for fn in self.before_first_request_fns:
                        yield fn
        for fn in self.before_request_fns:
            yield fn

    def after_request(self, fn):
        self.after_request_fns.append(fn)
        return fn

    def suggest_after_request(self, fn):
        self.suggest('after_request', fn)
        return fn

    def iter_after_request_handlers(self):
        for fn in self.after_request_fns:
            yield fn

    def provider(self, note, provider=None, name=False):
        if provider is not None:
            self._register_provider_data(
                'provider', note, provider=provider, name=name)
        else:
            def decorator(x):
                self._register_provider_data(
                    'provider', note, provider=x, name=name)
                return x
            return decorator

    def suggest_provider(self, note, provider, name=False):
        if provider is not None:
            self.suggest('provider', note, provider=provider, name=name)
        else:
            def decorator(x):
                self.suggest('provider', note, provider=x, name=name)
                return x
            return decorator

    def factory(self, note, fn=None):
        if fn is not None:
            self._register_provider_data('factory', note, fn=fn)
        else:
            def decorator(f):
                self._register_provider_data('factory', note, fn=f)
                return f
            return decorator

    def suggest_factory(self, note, fn=None):
        if fn is not None:
            self.suggest('factory', note, fn=fn)
        else:
            def decorator(f):
                self.suggest('factory', note, fn=f)
                return f
            return decorator

    def value(self, note, scalar):
        self._register_provider_data('value', note, scalar=scalar)

    def suggest_value(self, note, scalar):
        self.suggest('value', note, scalar)

    def _register_provider_data(self, recipe_name, note, **recipe_keywords):
        self.provider_data[note] = recipe_name, recipe_keywords

    def iter_provider_data(self):
        for note, (recipe_name, recipe_keywords) in self.provider_data.items():
            yield recipe_name, note, recipe_keywords

    def suggest(self, recipe_name, *args, **kwargs):
        self.suggestions.append(Suggestion(recipe_name, *args, **kwargs))

    def iter_suggestions(self):
        for suggestion in self.suggestions:
            yield suggestion

    def take_suggestions(self, logger=None):
        if logger is None:
            log = lambda msg: None
        else:
            log = lambda msg: logger.info(msg)

        for plan in iter(self):
            for suggestion in plan.iter_suggestions():
                log('Taking suggestion: {!r}'.format(suggestion))
                suggestion(self)


class Suggestion(object):
    def __init__(self, name, *args, **kwargs):
        self.name = name
        self.args = args
        self.kwargs = kwargs

    def __call__(self, plan):
        method = getattr(plan, self.name)
        return method(*self.args, **self.kwargs)

    @staticmethod
    def format(*args, **kwargs):
        arg_sig = ', '.join(repr(arg) for arg in args)
        kwarg_sig = ', '.join('{}={}'.format(str(k), repr(v))
                              for k, v in kwargs.items())
        return ', '.join([x for x in (arg_sig, kwarg_sig) if x])

    def __repr__(self):
        arguments = self.format(*self.args, **self.kwargs)
        return 'plan.{}({})'.format(self.name, arguments)
