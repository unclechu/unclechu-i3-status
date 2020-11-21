{ pkgs ? import nix/default-nixpkgs-pin.nix
, name ? "unclechu-i3-status"
, src  ? ./.
}:
let
  unclechu-i3-status = import ./. { inherit pkgs name src; };
in
pkgs.haskellPackages.shellFor {
  packages = _: [
    unclechu-i3-status.haskellPackage
  ];

  withHoogle = true;

  buildInputs = [
    unclechu-i3-status
    pkgs.cabal-install
  ];
}
