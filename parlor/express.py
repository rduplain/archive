def build_delegating_method(delegate_name, method_name):
    def delegating_method(self, *a, **kw):
        delegate = getattr(self, delegate_name)
        method = getattr(delegate, method_name)
        return method(*a, **kw)
    doc_format = "Calls self.{}.{}."
    delegating_method.__name__ = method_name
    delegating_method.__doc__ = doc_format.format(delegate_name, method_name)
    return delegating_method


def delegate_methods(cls):
    for delegate_name in cls.delegated_methods:
        for method_name in cls.delegated_methods[delegate_name]:
            method = build_delegating_method(delegate_name, method_name)
            setattr(cls, method_name, method)
    return cls
