{ nixpkgs ? import ./nixpkgs.nix
, dev ? false
, githubPath ? "default"
}:
let
  ghPath =
    if githubPath == "default" then
      import ./nix/github.nix
    else
      githubPath;

  ghOverlay = self: super: {
    haskellPackages = super.haskellPackages.override (old: with super.haskell.lib; {
      overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper: {
        github = hsuper.callPackage ghPath {inherit nixpkgs;};
      });
    });
  };

  pkgs = import nixpkgs {
    config.allowBroken = true;
    overlays = [
      (import "${ghPath}/overlay.nix")
      (import ./nix/overlay.nix)
      ghOverlay
    ];
  };
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
