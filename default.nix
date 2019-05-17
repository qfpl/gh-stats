{ nixpkgs ? import ./nixpkgs.nix
, dev ? false
}:
let
  pkgs = import nixpkgs {
    overlays = [(self: super: {
      haskellPackages = super.haskellPackages.override (oldHp: {
        overrides = self.lib.composeExtensions (oldHp.overrides or (_: _: {})) (hself: hsuper: {
          hw-rankselect = hself.callHackage "hw-rankselect" "0.12.0.4" {};
        });
      });
    })];
  };

  drv = pkgs.haskellPackages.callPackage ./gh-stats.nix {};

  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      [ (pkgs.haskellPackages.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
        pkgs.cabal-install
        pkgs.haskellPackages.ghcid
      ];
  });
in
  if dev then shellDrv else drv
