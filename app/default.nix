with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ffdb11a.tar.gz";
  sha256 = "0fhg09599qlx92nk1lrkifiqg39lbllacxkvzha5crbl02ly48gj";  # obtained with nix-prefetch-url --unpack <URL>
}) {});
let
  dev = pkgs.haskell.packages.ghc8107.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};
in {
  dev = dev.overrideAttrs (old: {
    postInstall = ''
      mkdir -p $out/bin/assets
      cp -r ${old.src}/assets $out/bin/app.jsexe
    '';
  });
  release = release.overrideAttrs (old: {
    postInstall = ''
      cp -r ${old.src}/assets $out/bin/app.jsexe
    '';
  });
  inherit pkgs;
}
