# Prepare Build

At the repo's root:

```bash
mkdir -p app/assets
./scripts/GenAssets.hs
./scripts/dl-large-assets.sh
```

You only need to that the first time you build the project. If `GenAssets.hs`
or `dl-large-assets.sh` need to be executed again, it'll be specified
in commits messages (read them when you `fetch`).

# Build

Build release with `rm .ghc.environment.* -Rf; nix-build` (from the `app`
directory). This puts the result in `result-2/bin/app.jsexe/`.

Or:

```shell
# To avoid building ghcjs, enable caching (from miso's README) (do it only once)
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
# Then enter the nix shell:
nix-shell -A release.env default.nix
cabal --project-file=cabal.config build all
```

# Formatting

Haskell code in miso-darkcraw must be formatted with
[Ormolu 0.4.0.0](https://github.com/tweag/ormolu/releases/tag/0.4.0.0).
The [pre-commit hook](https://github.com/smelc/miso-darkcraw/blob/master/hooks/pre_commit.py)
automatically amends commits to honor formatting.

# Documentation

## Online version

The haddock is automatically updated by
the [pre-commit hook](https://github.com/smelc/miso-darkcraw/blob/master/hooks/pre_commit.py) and
is visible online [here](https://smelc.github.io/miso-darkcraw/).

## Local version

To generate the documentation of Haskell files:

```shell
nix-shell
cabal --project-file=cabal-haddock.config haddock app
# Documentation gets created in dist-newstyle/build/x86_64-linux/ghc-8.6.5/app-0.1.0.0/x/app/doc/html/app/app/index.html
hoogle generate --local
```

# neovim integration

To have GHC feedback within `neovim`:

```
# This installs hls locally in the "hls/" directory
./scripts/install-hls.sh 1.3.0  # 1.3.0 is the version of HLS at the time of writing
export PATH="$(pwd)/hls:$PATH"

cd app
haskell-language-server-wrapper
# lot of output, should work... kill it
nix-shell  # Enter environment defined by shell.nix
nvim shared/Card.hs
```

It seems [coc.nvim](https://github.com/neoclide/coc.nvim) works
with my usual configuration, nice! Make sure it has:

```
  "languageserver": {
    "haskell": {
      "command": "haskell-language-server-wrapper",
```

so that it matches the filenames above in `hls/`.

# [ghcid](https://github.com/ndmitchell/ghcid)

```shell
nix-shell
cabal install ghcid
ghcid  # See the .ghcid for the --command used
ghcid | source-highlight -s haskell -f esc  # To have syntax highlighting in error messages
```

# [hlint](https://github.com/ndmitchell/hlint)

`hlint $(git ls-files "*.hs")`

# repl

To launch a repl for the application's context, do:

```
cabal repl app.cabal:app
```

For a repl for the test context, do:

```
nix-shell
cabal repl test-darkcraw
```

Then to execute a single test:

```
> import Test.Hspec
> import Main
> hspec testPlaceCommutation -- for example
```

To profile (for example to debug when it doesn't terminate):

```
cabal install profiteur  # Creates ~/.cabal/bin/profiteur
cabal run --enable-profiling test-darkcraw -- +RTS -p
~/.cabal/bin/profiteur test-darkcraw.prof
```

To profile when it doesn't terminate or when a backtrace is missing,
add that to `app.cabal` in the the `test-darkcraw` section:

```
  ghc-options:
    -fprof-auto
    -prof
    -rtsopts
```

Run a test as follows:

```
./dist-newstyle/build/x86_64-linux/ghc-8.6.5/app-0.1.0.0/t/test-darkcraw/build/test-darkcraw/test-darkcraw +RTS -xc
```

If debugging doesn't terminate, because the code loops, beware that
the backtrace may only happen when `Ctrl-c`ing `test-darkcraw`

# Fast feedback

* To launch a `jsaddle` webserver that updates automatically:

  `./load-n-reload.sh` (`nix-shell --run reload` under the hood)
* To update the released version automatically (is closer to what players
  will observe because webserver has interaction issues):

  `./load-n-reload.sh release`

  It is faster than doing `nix-build` because it uses incremental compilation.
* To reexecute tests upon modifying them, execute:

  `./load-n-reload.sh test`

  It is faster than doing `nix-build` because it uses incremental compilation.

# Incremental ghcjs compilation with cabal

`nix-build` will recompile all modules at each invocation. To only recompile
what has changed since the last build, run:

```
nix-shell -A release.env default.nix --run "cabal --project-file=cabal.config build all"
```

# Seeing the module's dependency graph

* Install [`graphmod`](https://github.com/yav/graphmod)
* `touch shared/Main.hs`  (I didn't dig why graphmod wants to load this file)
* `graphmod $(git ls-files '*.hs') | tred | dot -Tpdf > modules.pdf`

# Tips

If `nix-build` fails with `ghc: can't find a package database at /home/churlin/.cabal/store/ghc-8.6.5/package.db`
then delete `app/.ghc.environment.*`. See:

* https://romanofskiat.wordpress.com/2019/02/07/ghc-cant-find-a-package-database/
* https://discourse.nixos.org/t/ghc-cant-find-a-package-database-at/3860/5

Remove derivations when disk is full:

* `rm -Rf /nix`
* `curl -L https://nixos.org/nix/install | sh`
* `nix-build`
