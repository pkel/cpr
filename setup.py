from setuptools import setup, Extension

setup(
    name="cpr",
    packages=["cpr_gym"],
    package_dir={"cpr_gym": "./gym/cpr_gym"},
    package_data={"cpr_gym": ["bridge.so"]},
    ext_modules=[Extension(name="force-platform-wheel", sources=[])],
    install_requires=["gym", "numpy"],
)
