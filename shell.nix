let sources = import nix/sources.nix; in
# Forwarded arguments
args@
{ pkgs ? import sources.nixpkgs {}

# Forwarded overridable dependencies
, __nix-utils ? pkgs.callPackage sources.nix-utils {}

# Forwarded build options
, __src ? null
, justStaticExecutable ? false # N.B. Default value is different here

# Local arguments
, withCabal ? false
, withStack ? false
, withPackageRepl ? false # Adds package library modules into GHCi REPL
, withHoogle ? true
, buildExecutable ? true
}:
let
  forwardedNames = [ "__nix-utils" "__src" ];
  filterForwarded = pkgs.lib.filterAttrs (n: v: builtins.elem n forwardedNames);
  forwardedArgs = { inherit justStaticExecutable; } // filterForwarded args;
  pkg = pkgs.callPackage ./. forwardedArgs;
  hp = pkg.haskellPackages;
  name = pkg.haskellPackage.pname;

  inherit (__nix-utils) wrapExecutable;
  pkgReplGhc = hp.ghcWithPackages (p: [p.${name}]);

  # Produces ‘PACKAGE-NAME-ghc’ and ‘PACKAGE-NAME-ghci’ files.
  # ‘shellFor’ overrides ‘ghc’ and ‘ghci’ executables.
  pkgRepl =
    let
      exe = binName:
        wrapExecutable
          "${pkgReplGhc}/bin/${binName}"
          { name = "${name}-${binName}"; };
    in [
      (exe "ghci")
      (exe "ghc")
    ];
in
hp.shellFor {
  packages = p: [
    p.${name}
  ];

  inherit withHoogle;

  buildInputs =
    (if withCabal then [ hp.cabal-install ] else []) ++
    (if withStack then [ hp.stack ] else []) ++
    (if buildExecutable then [ hp.${name} ] else []) ++
    (if withPackageRepl then pkgRepl else []);
}
