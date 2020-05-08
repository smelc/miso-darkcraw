#!/usr/bin/env python3
#
# Script to be executed before commiting, to do things fast.
#
# Installation: ln -sr pre-commit.py .git/hooks/pre-commit
#
# You can pass "--unstaged" to apply the hook on modified but not yet
# staged files. This is useful to apply the hook prior staging.

import subprocess
import sys
from typing import List


def _git_diff(staged_or_modified: bool, extension: str) -> List[str]:
    """
    Args:
        extension: the extension of files considered, such as "py" or "ml"
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
    Returns: A list of relevant versioned files that are staged or modified
    """
    git_cmd = ["git", "diff"]
    if staged_or_modified:
        git_cmd += ["--cached"]
    git_cmd += ["--name-only", "--diff-filter=ACMR", "*." + extension]
    git_diff_result = subprocess.run(git_cmd,
                                     stdout=subprocess.PIPE,
                                     universal_newlines=True)
    if git_diff_result.returncode != 0:
        print("Call to git diff failed, failing globally.", file=sys.stderr)
        sys.exit(1)
    # The comprehension filters empty lines
    return [x for x in git_diff_result.stdout.split("\n") if x]


def _call_tool(files, staged_or_modified: bool, cmd: list) -> int:
    """
    Args:
        files (list(str)): The files on which to call a tool (which is expected
                           to modify these files)
        staged_or_modified (bool) Whether staged files are considered (True)
                                  or modified ones (False)
        cmd (list(str)): The command to call the considered tool. The path to a
                         given file is appended to it before executing it.
    Returns:
        The maximum of return codes of calls to the considered tool on `files`
    """
    if staged_or_modified:
        # If we're dealing with staged files, we don't want to call
        # ocamlformat if the file has unstaged modifications; because
        # adding (in git) the file after having formatted would stage
        # those modifications
        cmd = ["git", "diff", "--name-only"]
        modified_files_result = subprocess.run(cmd,
                                               stdout=subprocess.PIPE,
                                               universal_newlines=True)
        if modified_files_result.returncode != 0:
            print("Call to %s failed" % " ".join(cmd), file=sys.stderr)
            sys.exit(1)
        trimmed_files = [
            x for x in files
            if x not in modified_files_result.stdout.split("\n")
        ]
        excluded = [x for x in files if x not in trimmed_files]
        if excluded:
            print(
                """Some files are not considered because they have unstaged modifications.
Modifying them and readding them would stage unwanted modifications.
Concerned files:""")
            for x in excluded:
                print("  " + x)
        files = trimmed_files
    rc = 0
    for file_ in files:
        cmd = list(cmd)  # Copy to be safe
        cmd.append(file_)
        print(" ".join(cmd))
        call_result = subprocess.run(cmd,
                                     stdout=subprocess.PIPE,
                                     universal_newlines=True)
        if call_result.returncode != 0:
            print("Call to %s failed " % " ".join(cmd), file=sys.stderr)
            sys.exit(1)
        if staged_or_modified:
            # Readd file, so that formatting makes it to the commit
            # This is safe, because of the previous check having
            # no unstaged modification. Hence adding it only stages
            # formatting changes
            #
            # On another topic, we have no way to know if ocamlformat did
            # a modification. Hence we're always readding. If we had
            # this information, we would be able to avoid these calls.
            cmd = ["git", "add", file_]
            print(" ".join(cmd))
            git_result = subprocess.run(cmd)
            if git_result.returncode != 0:
                print("Call to %s failed " % " ".join(cmd), file=sys.stderr)
                sys.exit(1)
    return rc


def main() -> int:
    staged = "--unstaged" not in sys.argv
    adjective = "staged" if staged else "modified"

    rc = 0

    relevant_hs_files = _git_diff(staged, "hs")
    # Remove Main.hs that uses CPP
    relevant_hs_files = [x for x in relevant_hs_files if "Main.hs" in x]
    if relevant_hs_files:
        ormolu_rc = _call_tool(relevant_hs_files, staged,
                               ["ormolu", "--inplace"])
        rc = max(rc, ormolu_rc)
    else:
        print("No %s *.hs relevant file found" % adjective)

    return rc


if __name__ == "__main__":
    sys.exit(main())