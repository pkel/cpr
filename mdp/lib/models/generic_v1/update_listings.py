import inspect
import os
import re
import subprocess
from . import protocols

dst = os.path.dirname(__file__) + "/listings"


def filter_source(txt):
    acc = []
    lns = txt.splitlines()[1:]
    indent = re.match(r"\s+", lns[0])[0]
    for ln in txt.splitlines()[1:]:
        ln = ln.replace(indent, "", 1)
        ln = re.sub(r"^(def [a-zA-Z-9_]*)\(self,? ?", "\\1(", ln)
        ln = re.sub(r"self\.", "", ln)
        if re.match(r"\s+assert.*", ln):
            continue
        acc.append(ln)
    return "\n".join(acc)


def format_source(path):
    subprocess.run(["black", path])


def main():
    os.makedirs(dst, exist_ok=True)
    for x in dir(protocols):
        if x.startswith("_"):
            continue
        y = getattr(protocols, x)
        if hasattr(y, "Listing"):
            path = f"{dst}/{x}.py"
            print(f"write {path}")
            src = inspect.getsource(getattr(y, "Listing"))
            with open(path, "w") as f:
                f.write(filter_source(src))
                f.write("\n")
            format_source(path)
        if hasattr(y, "Util"):
            path = f"{dst}/{x}_util.py"
            print(f"write {path}")
            src = inspect.getsource(getattr(y, "Util"))
            with open(path, "w") as f:
                f.write(filter_source(src))
                f.write("\n")
            format_source(path)


if __name__ == "__main__":
    main()
else:
    raise RuntimeError("this should be run as script!")
