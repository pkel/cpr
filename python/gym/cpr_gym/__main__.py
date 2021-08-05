import argparse
import cpr_gym
import urllib.request
import shutil


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--update", action="store_true", help="update bridge.so")
    args = parser.parse_args()
    if args.update:
        print(f"downloading bridge.so  to {cpr_gym.dll_name}_new")
        urllib.request.urlretrieve(cpr_gym.dll_url, cpr_gym.dll_name + "_new")
        print(f"moving {cpr_gym.dll_name} -> {cpr_gym.dll_name}_old")
        shutil.move(cpr_gym.dll_name, cpr_gym.dll_name + "_old")
        print(f"moving {cpr_gym.dll_name}_new -> {cpr_gym.dll_name}")
        shutil.move(cpr_gym.dll_name + "_new", cpr_gym.dll_name)
