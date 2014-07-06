import logging

from jeni import annotate


@annotate
def logger_factory(
        name: annotate.maybe('config:LOGGER_NAME')=__name__,
        level: annotate.maybe('config:LOGGER_LEVEL')=logging.INFO,
        handler: annotate.maybe('config:LOGGER_HANDLER')=None):
    logger = logging.getLogger(name)
    logger.setLevel(level)
    if not logger.hasHandlers():
        if handler is None:
            handler = logging.StreamHandler()
        logger.addHandler(handler)
    return logger
