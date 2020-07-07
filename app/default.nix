with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/bb23019.tar.gz";
  sha256 = "0q44lxzz8pp89ccaiw3iwczha8x2rxjwmgzkxj8cxm97ymsm0diy";  # obtained with nix-prefetch-url --unpack <URL>
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
