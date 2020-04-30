Result is in `result/bin/app.jsexe/`. To produce it: `nix-build`

To see it realtime: `localhost:8080`

# neovim integration

To have GHC feedback within `neovim`:

```
nix-shell  # Enter environment defined by shell.nix
nvim Main.hs
```

It seems [coc.nvim](https://github.com/neoclide/coc.nvim) works
with my usual configuration, nice!

# Fast feedback

To launch [`ghcid`](https://github.com/ndmitchell/ghcid) in a terminal
and update the webserver automatically:

`nix-shell --run reload`

# Tips

If `nix-build` fails with `ghc: can't find a package database at /home/churlin/.cabal/store/ghc-8.6.5/package.db`
then delete `app/.ghc.environment.*`. See:

* https://romanofskiat.wordpress.com/2019/02/07/ghc-cant-find-a-package-database/
* https://discourse.nixos.org/t/ghc-cant-find-a-package-database-at/3860/5
