import copy

from .app import Application
from .express import delegate_methods
from .plan import Plan


@delegate_methods
class Builder(object):
    plan_class = Plan
    default_application_class = Application

    delegated_methods = {
        'plan': [
            'add_url_rule',
            'route',
            'errorhandler',
            'before_request',
            'before_first_request',
            'after_request',
            'provider',
            'factory',
            'value',
        ],
    }

    def __init__(self, config=None, application_class=None):
        self.set_application_class(application_class)
        self.plan = self.plan_class(config=config)

    def iter_plans(self):
        yield self.plan.copy()

    def copy(self):
        builder = self.__class__(application_class=self.application_class)
        builder.plan = self.plan.copy()
        return builder

    def build_application(self, application_class=None):
        if application_class is None:
            application_class = self.application_class
        return application_class(plans=list(self.iter_plans()))

    def set_application_class(self, application_class):
        if application_class is None:
            self.application_class = self.default_application_class
        else:
            self.application_class = application_class


class Crew(Builder):
    def __init__(self, *builder_items, config=None, application_class=None):
        self.set_application_class(application_class)
        self.plan = self.plan_class(config=config)
        self.builder_items = builder_items

    def copy(self):
        crew = self.__class__(application_class=self.application_class)
        crew.plan = self.plan.copy()
        crew.builders = copy.copy(self.builders)
        return crew

    def iter_plans(self):
        crew_plan = self.plan.copy()
        for prefix, builder in self.builder_items:
            for plan_copy in builder.iter_plans():
                plan_copy.set_prefix(prefix)
                plan_copy.update(crew_plan)
                yield plan_copy
        yield crew_plan
