import logging

from parlor import Plan
from jeni import Provider, annotate


plan1 = Plan()


@annotate
@plan1.route('/')
def index(
        hello: 'hello',
        eggs: 'eggs'):
    return str((hello, eggs))


@plan1.before_request
@annotate
def before1(logger: 'logger'):
    logger.info('@plan1.before_request')


@plan1.route('/foo/')
def foo1():
    return "This is not the foo you're looking for."


@annotate
@plan1.route('/bar/<int:num>/')
def bar(num: 'arg:num'):
    return 'bar\n' * num


plan2 = Plan()


@annotate
@plan2.route('/other/')
def other_route(hello: 'hello:thing', eggs: 'eggs'):
    return str((hello, eggs))


@plan2.before_request
@annotate
def before2(logger: 'logger'):
    logger.info('@plan2.before_request')


@plan2.route('/error/')
def error_route():
    raise Exception('Nope.')


plan3 = Plan()


@annotate
@plan3.route('/')
def session_route(session: 'session'):
    return str(session)


@annotate
@plan3.route('/start/')
def session_start_route(session_start: 'session_start'):
    return session_start(user_uid='c0ffee')


@annotate
@plan3.route('/set/')
def session_set_route(session: 'session', args: 'args'):
    # Does not persist if session has not been started.
    session.update(args)
    return str(session)


@annotate
@plan3.route('/end/')
def session_end_route(session_end: 'session_end'):
    return session_end()


plan = Plan(
    ('/', plan1),
    ('/', plan2),
    ('/session', plan3),
    LOGGER_LEVEL=logging.DEBUG,
    SECRET_KEY='secret',
    SQL_DATABASE_URL='sqlite:///demo.db')


@plan.route('/foo/')
def foo0():
    return "This is the right foo."


@plan.before_request
@annotate
def before3(logger: 'logger'):
    logger.info('@plan.before_request')


@plan.provider('hello')
class HelloProvider(Provider):
    def get(self, name=None):
        if name is None:
            name = 'world'
        return 'Hello, {}!'.format(name)


@plan.factory('eggs')
def eggs():
    return 'eggs!'


def build_application():
    from parlor import Injector
    from parlor.app.flask import FlaskApplication
    from parlor.session import init as session_init
    from sqlalchemy.ext.declarative import declarative_base

    Base = declarative_base()
    class Application(FlaskApplication):
        injector_class = Injector

    session_init(Injector, Base, sql_ns='sql')

    application = Application(plan)
    with application.injector_class() as injector:
        Base.metadata.create_all(injector.get('sql_engine'))
    return application


if __name__ == '__main__':
    application = build_application()
    app = application.get_implementation()
    app.run(debug=True)
