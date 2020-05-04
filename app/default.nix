with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
let
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
in
  let
    dev2 = dev.overrideAttrs (old: {
    postInstall = ''
        mkdir -p $out/bin/assets
        cp -r ${old.src}/assets $out/bin/app.jsexe
      '';
  });
    release2 = release.overrideAttrs (old: {
    postInstall = ''
        cp -r ${old.src}/assets $out/bin/app.jsexe
      '';
  });
  in {
    inherit dev2;
    inherit release2;
    inherit pkgs;
  }
