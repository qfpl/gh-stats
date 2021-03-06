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

  activate = pkgs.writeScriptBin "activate" ''
    #!${pkgs.bash}/bin/bash -e

    systemctl restart gh-stats-server.service
    '';

  drvRaw = hp.callPackage ./gh-stats.nix {};
  drv = pkgs.stdenv.lib.overrideDerivation drvRaw (attrs: {
    postInstall = ''
      ${attrs.postInstall or ""}
      ln -sv ${activate}/bin/activate $out/bin
    '';
  });

  shellDrv = pkgs.haskell.lib.overrideCabal drvRaw (drv': {
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
