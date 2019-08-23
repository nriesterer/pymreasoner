from setuptools import setup

import mreasoner

setup(
    name='mreasoner',
    version=mreasoner.__version__,
    description='Python interface for mReasoner',
    author='Nicolas Riesterer',
    author_email='riestern@cs.uni-freiburg.de',
    url='https://github.com/nriesterer/pymreasoner',
    packages=['mreasoner'],
    install_requires=['scipy']
)
