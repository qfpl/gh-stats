{ nixpkgs ? import ./nixpkgs.nix
, dev ? false
, githubPath ? "nixpkgs"
}:
let
  githubOverrides = hself:
    if githubPath == "nixpkgs" then
      {}
    else
      {
        github = hself.callCabal2nix "github" githubPath {};
      };

  config = rec {
    # sv and hw-rankselect are marked broken in this nixpkgs
    allowBroken = true;
    packageOverrides = pkgs: with pkgs.haskell.lib; rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          hw-prim = doJailbreak super.hw-prim;
          hw-bits = doJailbreak super.hw-bits;
          hw-rankselect-base = doJailbreak super.hw-rankselect-base;
          hw-rankselect = doJailbreak (self.callHackage "hw-rankselect" "0.13.0.0" {});
          hw-excess = doJailbreak super.hw-excess;
          hw-balancedparens = doJailbreak super.hw-balancedparens;
          sv-core = self.callHackage "sv-core" "0.4.1" {};
        } // githubOverrides self;
      };
    };
  };

  pkgs = import nixpkgs { inherit config; };
  hp = pkgs.haskellPackages;

  drv = hp.callPackage ./gh-stats.nix {};

  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      [ (hp.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
        pkgs.cabal-install
        hp.ghcid
      ];
  });
in
  if dev then shellDrv else drv
