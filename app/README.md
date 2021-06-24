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

# neovim integration

To have GHC feedback within `neovim`:

```
# At the root of the repo
mkdir hls
cd hls
# This mentions release 1.2.0, use a newer version if available!
wget https://github.com/haskell/haskell-language-server/releases/download/1.2.0/haskell-language-server-Linux-8.6.5.gz
wget https://github.com/haskell/haskell-language-server/releases/download/1.2.0/haskell-language-server-wrapper-Linux.gz
gunzip *.gz
mv haskell-language-server-Linux-8.6.5 haskell-language-server-8.6.5
mv haskell-language-server-wrapper-Linux haskell-language-server-wrapper
cd .. # back to the repo's root
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

* To launch [`ghcid`](https://github.com/ndmitchell/ghcid) in a terminal
  that will update the webserver automatically:

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
