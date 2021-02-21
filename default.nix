let sources = import nix/sources.nix; in
{ pkgs  ? import sources.nixpkgs {}
, utils ? import sources.nix-utils { inherit pkgs; }
, src   ? (import nix/clean-src.nix { inherit pkgs; }) ./.
, justStaticExecutable ? true
}:
let
  inherit (utils) wrapExecutable;

  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    ${name} = pkg;
  });

  name = "unclechu-i3-status";
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
  pkg-exe = "${justStaticExecutableFn pkg}/bin/${name}";

  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';

  justStaticExecutableFn =
    if justStaticExecutable
    then pkgs.haskell.lib.justStaticExecutables
    else x: x;

  wrapper = wrapExecutable pkg-exe {
    deps = [ pkgs.dzen2 ];
    inherit checkPhase;
  };
in
wrapper // { inherit src haskellPackages; haskellPackage = pkg; }
