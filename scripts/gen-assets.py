#!/usr/bin/env python3
#
# Script to split assets/* and populate app/assets/ with it

import os
import subprocess
import sys
from typing import List


def get_image_size(filename: str, width_or_height: bool) -> int:
    arg = "%w" if width_or_height else "%h"
    cmd = ["identify", "-format", arg,
           filename]  # install imagemagick to have this command
    print(" ".join(cmd))
    result = subprocess.run(cmd,
                            stdout=subprocess.PIPE,
                            universal_newlines=True)
    if result.returncode != 0:
        print(f"%s failed ({str(result.returncode)})" % " ".join(cmd))
        return result.returncode
    return int(result.stdout)


def delete_if_transparent(filepath: str):
    # Deletes 'filepath' if it's a completely transparent image
    if not os.path.exists(filepath):
        print(f"{filepath} should exist")
        return 1
    cmd = [
        "convert", filepath, "-channel", "a", "-separate", "-scale", "1x1!",
        "-format", "%[fx:mean]", "info:"
    ]
    print(" ".join(cmd))
    result = subprocess.run(cmd,
                            stdout=subprocess.PIPE,
                            universal_newlines=True)
    if result.returncode != 0:
        print(f"%s failed ({str(result.returncode)})" % " ".join(cmd))
        return result.returncode
    if result.stdout != "0":  # Not totally transparent
        return 0
    print(f"rm {filepath}")
    os.remove(filepath)
    return 0


def split_tiles(size: int) -> int:
    input_file = os.path.join("assets", f"{str(size)}x{str(size)}.png")
    if not os.path.exists(input_file):
        print(f"{input_file} must exist", file=sys.stderr)
        return 1
    width = get_image_size(input_file, True)
    height = get_image_size(input_file, False)
    if width % size != 0:
        print(
            f"{input_file}'s width should be a multiple of {size}, but it is {width}'",
            file=sys.stderr)
        return 1
    if height % size != 0:
        print(
            f"{input_file}'s height should be a multiple of {size}, but it is {height}'",
            file=sys.stderr)
        return 1
    output_dir = os.path.join("app", "assets")
    if not os.path.isdir(output_dir):
        print(f"{output_dir} should be a directory", file=sys.stderr)
        return 1
    for x in range(0, int(width / size)):
        for y in range(0, int(height / size)):
            xoffset = x * size
            yoffset = y * size
            first_arg = f"{input_file}[{size}x{size}+{xoffset}+{yoffset}]"
            output_file = os.path.join(output_dir,
                                       f"{size}x{size}_{x}_{y}.png")
            cmd = ["convert", first_arg,
                   output_file]  # install imagemagick to have this command
            print(" ".join(cmd))
            result = subprocess.run(cmd)
            if result.returncode != 0:
                print(f"%s failed ({str(result.returncode)})" % " ".join(cmd))
                return result.returncode
            delete_if_transparent(output_file)

    return 0


def main() -> int:
    for size in [16, 24]:
        split_tiles(size)
    return 0


if __name__ == "__main__":
    sys.exit(main())
