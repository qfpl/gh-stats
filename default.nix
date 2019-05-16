{ nixpkgs ? import ./nixpkgs.nix
}:
let
  inherit (nixpkgs) pkgs;
in
  pkgs.haskellPackages.callPackage ./gh-stats.nix {}
