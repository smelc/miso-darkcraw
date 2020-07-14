# Build

Build release with `rm .ghc.environment.* -Rf; nix-build`. This puts
the result in `result-2/bin/app.jsexe/`.

# neovim integration

To have GHC feedback within `neovim`:

```
nix-shell  # Enter environment defined by shell.nix
nvim Main.hs
```

It seems [coc.nvim](https://github.com/neoclide/coc.nvim) works
with my usual configuration, nice!

# Fast feedback

* To launch [`ghcid`](https://github.com/ndmitchell/ghcid) in a terminal
and update the webserver automatically:

  `nix-shell --run reload`
* To do that AND to refresh the browser automatically
  (uses [midori](https://www.midori-browser.org/)):

  `./load-n-reload.sh`
* To update the released version automatically (is closer to what players
  will observe because webserver has interaction issues):

  `./load-n-reload.sh release`

  Contrary to the previous item, this script uses your default browser. It
  is faster than doing `nix-build` because it uses incremental compilation.
* To reexecute tests upon modifying them, execute:

  `./load-n-reload.sh test`

  It is faster than doing `nix-build` because it uses incremental compilation.

# Incremental ghcjs compilation with cabal

`nix-build` will recompile all modules at each invocation. To only recompile
what has changed since the last build, run:

```
nix-shell -A release.env default.nix --run "cabal --project-file=cabal.config build all"
```

# Tips

If `nix-build` fails with `ghc: can't find a package database at /home/churlin/.cabal/store/ghc-8.6.5/package.db`
then delete `app/.ghc.environment.*`. See:

* https://romanofskiat.wordpress.com/2019/02/07/ghc-cant-find-a-package-database/
* https://discourse.nixos.org/t/ghc-cant-find-a-package-database-at/3860/5
