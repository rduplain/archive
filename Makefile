export PYTHON_VERSION = 3.4

include .makefile_python
include .makefile_pypi

all: install flakes dist

README.html: README.rst rst2html-command
	rst2html $< > $@

install: python

clean:
	rm -fr README.html *.egg-info
	rm -fr env .ts-install
	rm -fr dist
