{ nixpkgs ? import ./nixpkgs.nix
}:

let
  inherit (nixpkgs) pkgs;

  drv = (import ./. {});

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
  shellDrv.env
