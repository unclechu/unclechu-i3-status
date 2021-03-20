# Author: Viacheslav Lotsmanov
# License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

let sources = import nix/sources.nix; in
# This module is supposed to be called with ‘nixpkgs.callPackage’
{ callPackage
, haskellPackages
, haskell
, dzen2

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __src ? (callPackage nix/clean-src.nix {}) ./. # A directory
, justStaticExecutable ? true
}:
let
  inherit (__nix-utils) wrapExecutable shellCheckers;

  name = "unclechu-i3-status";
  pkg = extendedHaskellPackages.callCabal2nix name __src {};
  pkg-exe = "${justStaticExecutableFn pkg}/bin/${name}";

  extendedHaskellPackages = haskellPackages.extend (self: super: {
    ${name} = pkg;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then haskell.lib.justStaticExecutables
    else x: x;

  checkPhase = ''
    ${shellCheckers.fileIsExecutable "${dzen2}/bin/dzen2"}
    ${shellCheckers.fileIsExecutable pkg-exe}
  '';
in
wrapExecutable pkg-exe {
  deps = [ dzen2 ];
  inherit checkPhase;
} // {
  inherit dzen2;
  haskellPackages = extendedHaskellPackages;
  haskellPackage = pkg;
}
