#!/usr/bin/env python

"""The setup script."""

from setuptools import setup, find_packages

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = ['Click>=7.0', ]

test_requirements = [ ]

setup(
    author="Megan Christina Davis",
    author_email='mdavis22@go.olemiss.edu',
    python_requires='>=3.6',
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
    ],
    description="python library to make QFF processing less soul crushing",
    entry_points={
        'console_scripts': [
            'qff_helper=qff_helper.cli:main',
        ],
    },
    install_requires=requirements,
    license="MIT license",
    long_description=readme + '\n\n' + history,
    include_package_data=True,
    keywords='qff_helper',
    name='qff_helper',
    packages=find_packages(include=['qff_helper', 'qff_helper.*']),
    test_suite='tests',
    tests_require=test_requirements,
    url='https://github.com/MeganDavisChem/qff_helper',
    version='0.1.0',
    zip_safe=False,
)
