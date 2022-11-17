import argparse
import cpr_gym
import os
import requests
import shutil
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--update", action="store_true", help="update bridge.so")
    parser.add_argument(
        "--version", action="store_true", help="print bridge.so version"
    )
    args = parser.parse_args()
    if args.update:
        rel = requests.get(
            "https://api.github.com/repos/pkel/cpr/releases/latest"
        ).json()
        tag = rel["tag_name"]
        if sys.platform not in ["linux", "darwin"]:
            raise OSError(
                f"Unsupported OS: '{sys.platform}' - we support Linux and MacOS"
            )
        repo_url = "https://github.com/pkel/cpr"
        dll_url = f"{repo_url}/releases/download/{tag}/cpr-{tag}-{sys.platform}.so"

        if os.path.exists(cpr_gym.dll_name):
            print(f"backup bridge to {os.path.relpath(cpr_gym.dll_name)}_old")
            shutil.move(cpr_gym.dll_name, cpr_gym.dll_name + "_old")

        print(f"download new bridge from {dll_url}")
        so = requests.get(dll_url, stream=True)
        so.raise_for_status()
        with open(f"{cpr_gym.dll_name}", "wb") as handle:
            for block in so.iter_content(1024):
                handle.write(block)

    if args.version:
        print(cpr_gym.engine.cpr_lib_version)
