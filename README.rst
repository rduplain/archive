======================================
 parlor gathers dependencies & routes
======================================

parlor defines an `Application` class, tools for planning and running
`Application` instances, and provides injection of dependencies across
`Application` implementations. The `Application` class assumes a
request-response style interaction, but does not itself implement request
handling; this is left to concrete Application implementations.

1. Plan application functionality using builders.
2. Combine builders into crews if needed; crew configuration overrides builder.
3. Instantiate the Application for command-line, WSGI, or other service.

Based on `jeni <http://pypi.python.org/pypi/jeni>`_.


Dependencies
------------

Installing parlor only installs its core dependencies. To use features
available as extras, provide the extra names (see setup.py) when installing::

    pip install parlor[flask,sql]

This pattern can also be used in setup.py and requirements.txt declarations,
and can include a version spec for parlor, e.g. ``parlor[sql]>=0.2``.


License
-------

Copyright 2013-2014 Ron DuPlain <ron.duplain@gmail.com>.

Released under the BSD License (see LICENSE file).
