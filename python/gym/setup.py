from setuptools import setup

setup(
    name="cpr_gym",
    packages=['cpr_gym'],
    package_data={'cpr_gym': ['bridge.so']},
    install_requires=['gym', 'numpy']
    )
