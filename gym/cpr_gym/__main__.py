import argparse
import cpr_gym
import os

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--version", action="store_true", help="print version")
    args = parser.parse_args()

    if args.version:
        print(cpr_gym.__version__)
