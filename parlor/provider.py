from jeni import Provider, UnsetError


def dict_provider(dict_, require_key=True):
    class DictProvider(Provider):
        def get(self, name=None):
            if name is None:
                if require_key:
                    raise UnsetError()
                return dict_
            if name not in dict_:
                raise UnsetError()
            return dict_[name]
    return DictProvider


def multi_dict_provider(multi_dict, require_key=True):
    class MultiDictProvider(Provider):
        def get(self, name=None):
            if name is None:
                if require_key:
                    raise UnsetError()
                return multi_dict
            if name not in multi_dict:
                raise UnsetError()
            return multi_dict.getlist(name)
    return MultiDictProvider


def obj_provider(obj):
    class ObjProvider(Provider):
        def __init__(self):
            self.obj = obj
        def get(self, name=None):
            if name is None:
                return self.obj
            try:
                return getattr(self.obj, name)
            except AttributeError:
                raise UnsetError()
    return ObjProvider


def serialized_doc_provider(src, load, debug=False):
    class DocProvider(Provider):
        def __init__(self):
            try:
                self.doc = load(src)
            except Exception:
                if debug:
                    raise
                self.doc = None
                self.error = 'unable to load document'
            else:
                if self.doc is None:
                    self.error = 'document has no data'
                else:
                    self.error = None
        def get(self, name=None):
            if self.doc is None:
                raise UnsetError(self.error)
            if name is None:
                return self.doc
            if name not in self.doc:
                raise UnsetError()
            return self.doc[name]
    return DocProvider
