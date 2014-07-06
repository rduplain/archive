export PYTHON_VERSION = 3.4

include .makefile_python

all: install flakes

README.html: README.rst rst2html-command
	rst2html $< > $@

install: python

clean:
	rm -fr README.html *.egg-info
	rm -fr env .ts-install
