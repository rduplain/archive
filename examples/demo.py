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


plan = Plan(
    ('/', plan1),
    ('/', plan2), config={'LOGGER_LEVEL': logging.DEBUG})


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


if __name__ == '__main__':
    from parlor.app.flask import FlaskApplication
    application = FlaskApplication(plan)
    app = application.get_implementation()
    app.run(debug=True)
