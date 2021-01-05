let
  local = {
    # pinned
    nixpkgs  = fetchTarball https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz;
    universe = null;
    compiler = null;
  };

  # pinning done by static-haskell-nix
  static-haskell-nix = {
    nixpkgs  = fetchTarball https://github.com/NixOS/nixpkgs/archive/0c960262d159d3a884dadc3d4e4b131557dad116.tar.gz;
    universe = "pkgsMusl";
    compiler = "ghc865";
  };

  pkgs = import ./. { inherit (local) nixpkgs universe compiler; };
in
  pkgs.trait-lineariser.env
