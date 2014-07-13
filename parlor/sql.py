import sqlalchemy
import sqlalchemy.orm

from jeni import annotate


def init(injector_class, ns='sql'):
    """Initialize an Injector with namespaced SQLAlchemy objects.

    The namespace argument allows for multiple SQL engines to exist on a single
    injector. Annotations are prefixed with the given namespace (default:
    'sql').

    For example, using a database named 'foo' provides 'foo_engine',
    'foo_sessionmaker', 'foo_session', and 'foo_query':

    >>> import jeni
    >>> import parlor
    >>> import parlor.sql
    >>>
    >>> class Injector(jeni.Injector): pass
    ...
    >>> parlor.sql.init(Injector, 'foo')
    >>>
    >>> @Injector.factory('config')
    ... def config_factory(name=None):
    ...     if name == 'FOO_DATABASE_URL':
    ...         return 'sqlite://' # Provide the actual database URL.
    ...     raise jeni.UnsetError()
    ...
    >>> injector = Injector()
    >>> injector.get('foo_engine')
    Engine(sqlite://)
    >>> injector.get('foo_sessionmaker') # doctest: +ELLIPSIS
    sessionmaker(...bind=Engine(sqlite://)...)
    >>> injector.get('foo_session') # doctest: +ELLIPSIS
    <sqlalchemy.orm.session.Session object at 0x...>
    >>> injector.get('foo_query') # doctest: +ELLIPSIS
    <bound method Session.query of ...>
    >>>
    """

    def note(name):
        return '{}_{}'.format(ns, name)

    config_note = 'config:{}_DATABASE_URL'.format(ns.upper())

    @injector_class.factory(note('engine'))
    @annotate
    def engine_factory(uri: config_note):
        return sqlalchemy.create_engine(uri)

    @injector_class.factory(note('sessionmaker'))
    @annotate
    def sessionmaker_factory(engine: note('engine')):
        return sqlalchemy.orm.sessionmaker(engine)

    @injector_class.provider(note('session'))
    @annotate
    def session_provider(sessionmaker: note('sessionmaker')):
        session = sessionmaker()
        yield session
        session.close()

    @injector_class.factory(note('query'))
    @annotate
    def query_factory(session: note('session')):
        return session.query


if __name__ == '__main__':
    import doctest
    doctest.testmod()
