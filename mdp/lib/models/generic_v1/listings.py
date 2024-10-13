import inspect
import re
import subprocess

# class decorator for registering listings with a name
#
# @listing('bitcoin')
# class Bitcoin:
#     ...

_registry = dict()


def listing(name: str):
    def decorator(cls):
        if not isinstance(name, str):
            raise ValueError("listing name must be a string")

        if name in _registry:
            raise KeyError(f"duplicate listing name '{name}'")

        if not inspect.isclass(cls):
            raise TypeError("decorator can only be applied to classes")

        _registry[name] = cls
        return cls

    return decorator


# ---


def filter_source(txt):
    acc = []
    lns = txt.splitlines()[2:]  # skip decorator and class name
    indent = re.match(r"\s+", lns[0])[0]
    for ln in lns:
        ln = ln.replace(indent, "", 1)
        ln = re.sub(r"^(def [a-zA-Z-9_]*)\(self,? ?", "\\1(", ln)
        ln = re.sub(r"self\.", "", ln)
        if re.match(r"\s+assert.*", ln):
            continue
        acc.append(ln)
    return "\n".join(acc)


def format_source(txt):
    p = subprocess.run(["black", "-q", "-"], input=txt, capture_output=True, text=True)
    return p.stdout


def generate_listing(name: str, raw=False):
    if name not in _registry:
        raise KeyError(f"unknown listing '{name}'")

    src = inspect.getsource(_registry[name])

    if not raw:
        src = filter_source(src)
        src = format_source(src)

    return src
