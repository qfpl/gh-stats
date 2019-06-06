ghPath: self: super: {
  haskellPackages = super.haskellPackages.override (old: with super.haskell.lib; {
    overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper: {
      hw-prim = doJailbreak hsuper.hw-prim;
      hw-bits = doJailbreak hsuper.hw-bits;
      hw-rankselect-base = doJailbreak hsuper.hw-rankselect-base;
      hw-rankselect = doJailbreak (hself.callHackage "hw-rankselect" "0.13.0.0" {});
      hw-excess = doJailbreak hsuper.hw-excess;
      hw-balancedparens = doJailbreak hsuper.hw-balancedparens;
      sv-core = hsuper.callHackage "sv-core" "0.4.1" {};
      github = hsuper.callPackage ghPath {nixpkgs = super.path;};
    });
  });
}

