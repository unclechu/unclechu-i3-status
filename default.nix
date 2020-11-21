{ pkgs ? import nix/default-nixpkgs-pin.nix
, name ? "unclechu-i3-status"
, src  ? ./.
}:
let
  utils = import nix/utils-pin.nix { inherit pkgs; };
  inherit (utils) wrapExecutable;

  clean-fn = dir:
    pkgs.lib.cleanSourceWith {
      name = "${name}-source";
      src = pkgs.lib.cleanSource dir;
      filter = (fileName: fileType: ! (
        # exclude cabal stuff
        fileType == "directory" &&
        builtins.match "^dist(-newstyle)?$" (baseNameOf fileName) != null
      ));
    };

  clean-src = clean-fn src;
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";

  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';

  wrapper = wrapExecutable pkg-exe {
    deps = [ pkgs.dzen2 ];
    inherit checkPhase;
  };
in
wrapper // { inherit src; haskellPackage = pkg; }
