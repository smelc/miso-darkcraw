#!/usr/bin/env python3
"""
Script to be executed before commiting, to do things fast.

Installation: ln -sr hooks/pre_commit.py .git/hooks/pre-commit

You can pass "--unstaged" to apply the hook on modified but not yet
staged files. This is useful to apply the hook prior staging.
"""

import glob
import os
import shutil
import subprocess
import sys
import tempfile
from typing import List, Optional

_BUILD_TEST = False  # Whether to build and run tests in the precommit hook.
# Set to False since it's done by the CI.


def _git_diff(staged_or_modified: bool, extension: str) -> List[str]:
    """
    Args:
        extension: the extension of files considered, such as "py" or "ml"
        staged_or_modified: Whether to consider staged files (True)
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


def _call_tool(files: List[str], staged_or_modified: bool, cmd: list) -> int:
    """
    Args:
        files: The files on which to call a tool (which is expected
               to modify these files)
        staged_or_modified: Whether staged files are considered (True)
                            or modified ones (False)
        cmd: The command to call the considered tool. The path to a
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


def _run_cmd(cmd: List[str], cwd: Optional[str] = None, check: bool=False) -> int:
    """
    Args:
        cmd: The command to execute
        cwd: The working directory where to execute the command
        check: whether to fail with an Exception if the command fails
    Returns:
        The command's return code
    """
    prefix = f"{cwd}> " if cwd else ""
    print(f'{prefix}{" ".join(cmd)}')
    return subprocess.run(cmd, check=check, cwd=cwd).returncode


def _check_jsondata_dot_hs() -> int:
    """ Checks that ./th/main.py --check returns 0, i.e. that
        app/shared/JsonData.hs is up-to-date w.r.t to th/data.json """
    return _run_cmd(["./th/main.py", "--check"])


def _check_ormolu_version() -> int:
    """ Check that ormolu's version is the expected one. Returns a return
        code. """
    expected_version = "0.4.0.0"
    cmd = ["ormolu", "--version"]
    ormolu_output = subprocess.run(cmd,
                                   check=True,
                                   stdout=subprocess.PIPE,
                                   universal_newlines=True).stdout
    ormolu_output = ormolu_output.split(" ")
    version = ormolu_output[1] if len(ormolu_output) >= 2 else None
    if version is None:
        print(f"Unexpected ormolu --version output: {ormolu_output}")
        return 1
    elif version != expected_version:
        print(f"Unexpected ormolu version: {version}. Expected: {expected_version}")
        return 1
    else:
        return 0  # success case


def _check_roads_dot_hs() -> int:
    """ Checks that ./scripts/Roads.hs tiled/world.tmx /tmp/foo.hs
        create a foo.hs file that is similar to app/shared/Roads.hs """
    with tempfile.NamedTemporaryFile() as tmpfile:
        tmpfile = tmpfile.name
        _run_cmd(["./scripts/Roads.hs", "tiled/world.tmx", tmpfile], check=True)
        rc = _run_cmd(["diff", "app/shared/Roads.hs", tmpfile])
        return rc


def _build() -> int:
    """
    Build the project in release mode
    Returns: The return code of the build command
    """
    cmd = ["nix-build", "-A", "release"]
    return _run_cmd(cmd, cwd="app")


def _doc_should_be_rebuilt(staged_or_modified: bool, hs_files: List[str]) -> bool:
    """
    Args:
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
        hs_files (List[str]) The Haskell files that contain a change.
                             Not empty.
    Returns: whether the haddock should be rebuilt.
    """
    assert hs_files
    git_cmd = ["git", "diff", "--unified=0"]  # unified=0 to omit context lines
    if staged_or_modified:
        git_cmd += ["--staged"]
    git_cmd += hs_files
    git_diff_result = subprocess.run(git_cmd,
                                     check=True,
                                     stdout=subprocess.PIPE,
                                     universal_newlines=True)
    # The comprehension filters empty lines
    lines = [x for x in git_diff_result.stdout.split("\n") if x]
    lines = lines[4:]  # Omit first 4 lines, the ones looking like:
    # diff --git a/app/client/LootView.hs b/app/client/LootView.hs
    # index ed11c68..425b9b2 100644
    # --- a/app/client/LootView.hs
    # +++ b/app/client/LootView.hs
    lines = [x[1:] for x in lines if x[0] in ['-', '+']]  # Keep only lines starting with - or +,
    # i.e. omit lines starting with @@. Note that we know lines are not empty
    # by the filtering done above, so [0] is valid.

    # Doc should be rebuilt if a changed line contains "--" i.e. a Haskell
    # comment. This is slightly imprecise obviously, for example if {- ... -}
    # comments are used, but this is fine. The point is to avoid
    # rebuilding most of the time.
    return any(["--" in x for x in lines])


def _doc(staged_or_modified: bool, hs_files: List[str]) -> int:
    """
    Args:
        staged_or_modified (bool) Whether to consider staged files (True)
                                  or modified ones (False)
        hs_files (List[str]) The Haskell files that contain a change.
                             Not empty.
    Returns: a return code

    Builds the documentation (haddock) and copies it to
    the docs/ directory at the git root, then commits the changes (if any)
    """
    assert hs_files
    if not _doc_should_be_rebuilt(staged_or_modified, hs_files):
        print("No Haskell line containing '--' has been changed: not rebuilding Haddock 🐎")
        return 0  # Nothing to do

    cmd = ["nix-shell", "--run", "cabal --project-file=cabal-haddock.config haddock app"]
    rc = _run_cmd(cmd, cwd="app")
    if rc != 0:
        # A failure, it's fine, we just don't update the doc
        cmd_str = " ".join(cmd)
        print(f"⚠️ Could not build haddock with {cmd_str} ⚠️")
        return rc
    dest = "docs"
    if not os.path.exists(dest):
        os.makedirs(dest)
    # png and css produced by haddock are not writable O_o, hence to
    # avoid chown calls upon overwriting the doc, we only iterate over *.html
    for html_file in glob.glob("app/dist-newstyle/build/x86_64-linux/ghc-8.6.5/app-0.1.0.0/x/app/doc/html/app/app/*.html"):
        shutil.copy(html_file, dest)
        filename = os.path.basename(html_file)
        cmd = ["xmllint", "--format", "--html", filename, "-o", f"{filename}.tmp"]
        _run_cmd(cmd, cwd=dest, check=True)
        shutil.move(f"{dest}/{filename}.tmp", f"{dest}/{filename}")
        cmd = ["git", "add", filename]
        _run_cmd(cmd, cwd=dest, check=True)

    return 0


def _test() -> int:
    """
    Builds and executes tests
    Returns: The return code of building and executing the tests
    """
    cmd = ["nix-shell", "--run", "cabal test"]
    return _run_cmd(cmd, cwd="app")


def main() -> int:
    """ The main """
    staged = "--unstaged" not in sys.argv
    adjective = "staged" if staged else "modified"

    return_code = 0

    relevant_hs_files = _git_diff(staged, "hs")
    if relevant_hs_files:
        return_code = _check_ormolu_version()
        if return_code == 0:  # ormolu version is the expected one
            ormolu_rc = _call_tool(relevant_hs_files, staged,
                                   ["ormolu", "-m", "inplace"])
            return_code = max(return_code, ormolu_rc)
        if _BUILD_TEST:
            return_code = max(return_code, _build())
            return_code = max(return_code, _test())
        if return_code == 0:
            return_code = max(return_code, _doc(staged, relevant_hs_files))
    else:
        print(f"No {adjective} *.hs relevant file found, nothing to format, and no doc to generate either")
        if _BUILD_TEST:
            print("Not calling nix-build/cabal test either")

    return_code = max(return_code, _check_jsondata_dot_hs())
    return_code = max(return_code, _check_roads_dot_hs())

    return return_code


if __name__ == "__main__":
    sys.exit(main())
