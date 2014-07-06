import logging

from parlor import Builder, Crew
from jeni import Provider, annotate


builder = Builder()


@annotate
@builder.route('/')
def index(
        hello: 'hello',
        eggs: 'eggs'):
    return str((hello, eggs))


@builder.before_request
@annotate
def before1(logger: 'logger'):
    logger.info('@builder.before_request')


@builder.route('/foo/')
def foo1():
    return "This is not the foo you're looking for."


@annotate
@builder.route('/bar/<int:num>/')
def bar(num: 'arg:num'):
    return 'bar\n' * num


other = Builder()


@annotate
@other.route('/other/')
def other_route(hello: 'hello:thing', eggs: 'eggs'):
    return str((hello, eggs))


@other.before_request
@annotate
def before2(logger: 'logger'):
    logger.info('@other.before_request')


@other.route('/error/')
def error_route():
    raise Exception('Nope.')


crew = Crew(
    ('/', builder),
    ('/', other), config={'LOGGER_LEVEL': logging.DEBUG})


@crew.route('/foo/')
def foo0():
    return "This is the right foo."


@crew.before_request
@annotate
def before3(logger: 'logger'):
    logger.info('@crew.before_request')


@crew.provider('hello')
class HelloProvider(Provider):
    def get(self, name=None):
        if name is None:
            name = 'world'
        return 'Hello, {}!'.format(name)


@crew.factory('eggs')
def eggs():
    return 'eggs!'


if __name__ == '__main__':
    from parlor.app.flask import FlaskApplication
    application = crew.build_application(FlaskApplication)
    app = application.get_implementation()
    app.run(debug=True)
