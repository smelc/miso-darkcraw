#!/usr/bin/env python3
"""
Script to be executed before commiting, to do things fast.

Installation: ln -sr hooks/pre_commit.py .git/hooks/pre-commit

You can pass "--unstaged" to apply the hook on modified but not yet
staged files. This is useful to apply the hook prior staging.
"""

import subprocess
import sys
from typing import List, Optional

_BUILD_TEST = False  # Whether to build and run tests in the precommit hook.
# Set to False since it's done by the CI.


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
                                     check=True,
                                     stdout=subprocess.PIPE,
                                     universal_newlines=True)
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
        # If we're dealing with staged files, we don't want to format
        # if the file has unstaged modifications; because
        # adding (in git) the file after having formatted would stage
        # those modifications
        git_cmd = ["git", "diff", "--name-only"]
        modified_files_result = subprocess.run(git_cmd,
                                               check=True,
                                               stdout=subprocess.PIPE,
                                               universal_newlines=True)
        trimmed_files = [
            x for x in files
            if x not in modified_files_result.stdout.split("\n")
        ]
        excluded = [x for x in files if x not in trimmed_files]
        if excluded:
            print("Some files are not considered because"
                  " they have unstaged modifications."
                  "\nModifying them and readding them would stage unwanted"
                  " modifications\nConcerned files:")
            for exclude in excluded:
                print("  " + exclude)
        files = trimmed_files
    return_code = 0
    for file_ in files:
        tool_cmd = cmd + [file_]
        print(" ".join(tool_cmd))
        subprocess.run(tool_cmd, check=True)
        if staged_or_modified:
            # Readd file, so that formatting makes it to the commit
            # This is safe, because of the previous check having
            # no unstaged modification. Hence adding it only stages
            # formatting changes
            #
            # On another topic, we have no way to know if ocamlformat did
            # a modification. Hence we're always readding. If we had
            # this information, we would be able to avoid these calls.
            git_cmd = ["git", "add", file_]
            print(" ".join(git_cmd))
            subprocess.run(git_cmd, check=True)
    return return_code


def _run_unchecked_cmd(cwd: Optional[str], cmd: List[str]) -> int:
    """ Executes a command and returns it return code """
    prefix = f"{cwd}> " if cwd else ""
    print(f'{prefix}> {" ".join(cmd)}')
    return subprocess.run(cmd, check=False, cwd=cwd).returncode


def _check_jsondata_dot_hs() -> int:
    """ Checks that ./th/main.py --check returns 0, i.e. that
        app/shared/JsonData.hs is up-to-date w.r.t to th/data.json """
    return _run_unchecked_cmd(None, ["./th/main.py", "--check"])


def _build() -> int:
    """
    Build the project in release mode
    Returns: The return code of the build command
    """
    cmd = ["nix-build", "-A", "release"]
    return _run_unchecked_cmd("app", cmd)


def _test() -> int:
    """
    Builds and executes tests
    Returns: The return code of building and executing the tests
    """
    cmd = ["nix-shell", "--run", "cabal test"]
    return _run_unchecked_cmd("app", cmd)


def main() -> int:
    """ The main """
    staged = "--unstaged" not in sys.argv
    adjective = "staged" if staged else "modified"

    return_code = 0

    relevant_hs_files = _git_diff(staged, "hs")
    if relevant_hs_files:
        ormolu_rc = _call_tool(relevant_hs_files, staged,
                               ["ormolu", "-m", "inplace"])
        return_code = max(return_code, ormolu_rc)
        if _BUILD_TEST:
            return_code = max(return_code, _build())
            return_code = max(return_code, _test())
    else:
        print("No %s *.hs relevant file found, nothing to format" % adjective)
        if _BUILD_TEST:
            print("Not calling nix-build/cabal test either")

    return_code = max(return_code, _check_jsondata_dot_hs())

    return return_code


if __name__ == "__main__":
    sys.exit(main())
