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
, __x11-extras-src ? (callPackage nix/clean-src.nix {}) ./x11-extras # A directory
, justStaticExecutable ? true
}:
let
  inherit (__nix-utils) wrapExecutable shellCheckers;

  unclechu-i3-status =
    extendedHaskellPackages.callCabal2nix "unclechu-i3-status" __src {};
  unclechu-i3-status-exe =
    "${justStaticExecutableFn unclechu-i3-status}/bin/unclechu-i3-status";

  x11-extras =
    extendedHaskellPackages.callCabal2nix "x11-extras" __x11-extras-src {};

  extendedHaskellPackages = haskellPackages.extend (self: super: {
    inherit unclechu-i3-status x11-extras;
  });

  justStaticExecutableFn =
    if justStaticExecutable
    then haskell.lib.justStaticExecutables
    else x: x;

  checkPhase = ''
    ${shellCheckers.fileIsExecutable "${dzen2}/bin/dzen2"}
    ${shellCheckers.fileIsExecutable unclechu-i3-status-exe}
  '';
in
wrapExecutable unclechu-i3-status-exe {
  deps = [ dzen2 ];
  inherit checkPhase;
} // {
  inherit dzen2 x11-extras;
  haskellPackages = extendedHaskellPackages;
  haskellPackage = unclechu-i3-status;
}
