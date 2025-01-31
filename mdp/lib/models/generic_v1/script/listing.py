from .. import protocols  # trigger registry of listings # noqa: F401
from ..listings import generate_listing
import sys

if __name__ == "__main__":
    if len(sys.argv) < 2:
        raise RuntimeError("provide listing name as single argument")
    print(generate_listing(sys.argv[1]), end="")
else:
    raise RuntimeError("this should be run as script!")
