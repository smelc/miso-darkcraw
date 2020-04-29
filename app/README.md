# neovim integration

To have GHC feedback within `neovim`:

```
nix-shell  # Enter environment defined by shell.nix
nvim Main.hs
```

It seems [coc.nvim](https://github.com/neoclide/coc.nvim)
is capable of guessing on its own the environment, nice!

# ghcid

To launch [`ghcid`](https://github.com/ndmitchell/ghcid) in a terminal:

`nix-shell --run reload`
