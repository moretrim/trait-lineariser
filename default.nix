# This is an approach based on both [haskell-nix] and [static-haskell-nix].
#
# [haskell-nix]: https://github.com/Gabriel439/haskell-nix
# [static-haskell-nix]: https://github.com/nh2/static-haskell-nix

{ nixpkgs, universe ? null, compiler ? null }:

let
  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        let
          staticPackage = pkg: pkg.overrideAttrs (oldAttrs: {
            enableSharedLibraries = false;
            enableSharedExecutables = false;
            configureFlags = [
              "--ghc-option=-optl=-static"
              "--ghc-option=-optl=-pthread"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
            ];
          });

          configPackage =
            if builtins.elem universe ["pkgsStatic" "pkgsMusl" "pkgsSharedStatic"]
            then staticPackage
            else pkg: pkg;

          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            let
              toPackage = file: _: {
                name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

                value = let
                  pkg = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
                in configPackage pkg;
              };

            in
              pkgs.lib.mapAttrs' toPackage (builtins.readDir ./nix);

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };

          ghc =
            if builtins.isString compiler
            then pkgs.haskell.packages.${compiler}
            else pkgs.haskellPackages;
        in
          ghc.override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
    };
  };

  which =
    if builtins.isString universe
    then universe
    else "pkgs";
  pkgs = (import nixpkgs { inherit config; }).${which};

in
  { trait-lineariser = pkgs.haskellPackages.trait-lineariser;
  }
