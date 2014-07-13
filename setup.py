from os import path

from setuptools import setup


CLASSIFIERS = [
    'Development Status :: 3 - Alpha',
    'Intended Audience :: Developers',
    'License :: OSI Approved :: BSD License',
    'Operating System :: OS Independent',
    'Programming Language :: Python',
    'Programming Language :: Python :: 3',
    'Programming Language :: Python :: 3.4',
    'Topic :: Software Development :: Libraries :: Python Modules']


def extract_version(filepath=None, name='__version__'):
    """Parse __version__ out of Python file, default parlor/__init__.py."""
    if filepath is None:
        filepath = path.join('parlor', '__init__.py')
    context = {}
    for line in open(filepath):
        if name in line:
            exec(line, context)
            break
    else:
        raise RuntimeError('{} not found in {}'.format(name, filepath))
    return context[name]


README = 'README.rst'
with open(path.join(path.dirname(__file__), README)) as fd:
    long_description = '\n' + fd.read()


setup(
    name='parlor',
    version=extract_version(),
    url='https://github.com/rduplain/parlor',
    license='BSD',
    author='Ron DuPlain',
    author_email='ron.duplain@gmail.com',
    description='gathers dependencies & routes',
    long_description=long_description,
    packages=['parlor', 'parlor.app'],
    install_requires=[
        'Werkzeug>=0.9',
        'jeni>=0.3.1',
    ],
    extras_require={
        'flask': ['Flask>=0.10'],
        'sql': ['SQLAlchemy>=0.6'],
    },
    classifiers=CLASSIFIERS)
