{ nixpkgs ? import ./nixpkgs.nix
, dev ? false
, githubPath ? "github"
}:
let
  ghPath =
    if githubPath == "github" then
      import ./nix/github.nix
    else
      githubPath;

  pkgs = import nixpkgs {
    config.allowBroken = true;
    overlays = [
      (import "${ghPath}/overlay.nix")
      (import ./nix/overlay.nix ghPath)
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
