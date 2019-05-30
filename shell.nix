{ nixpkgs ? import ./nixpkgs.nix
, githubPath ? "nixpkgs"
}:

(import ./. {dev = true; inherit githubPath;}).env
