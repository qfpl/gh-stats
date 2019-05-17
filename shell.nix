{ nixpkgs ? import ./nixpkgs.nix
}:

(import ./. {dev = true;}).env
