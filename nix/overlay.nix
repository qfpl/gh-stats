self: super: {
  haskellPackages = super.haskellPackages.override (old: with super.haskell.lib; {
    overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper: {
      # Newer versions we need.
      concurrent-output = hsuper.concurrent-output_1_10_10;
      sv-core = hsuper.callHackage "sv-core" "0.4.1" {};

      # Things that work with our newer versions of things.
      cassava = doJailbreak hsuper.cassava;
      hw-prim = doJailbreak hsuper.hw-prim;
      hw-bits = doJailbreak hsuper.hw-bits;
      hw-rankselect-base = doJailbreak hsuper.hw-rankselect-base;
      hw-rankselect = doJailbreak (hself.callHackage "hw-rankselect" "0.13.0.0" {});
      hw-excess = doJailbreak hsuper.hw-excess;
      hw-balancedparens = doJailbreak hsuper.hw-balancedparens;
      psqueues = doJailbreak hsuper.psqueues;
      vault = doJailbreak hsuper.vault;

      # Revision 1 removes upper bound on hashable. This is what latest hackage-packages.nix has.
      # We missed the update by a couple of days.
      uniplate = overrideCabal hsuper.uniplate (old: {
        sha256 = "1dx8f9aw27fz8kw0ad1nm6355w5rdl7bjvb427v2bsgnng30pipw";
        revision = "1";
        editedCabalFile = "0gsrs2mk58jg3x36dyzxi4y46isd5p6q0rd6m9l834h5r7ds6a54";
      });
    });
  });
}

