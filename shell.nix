{ nixpkgs ? import ./nixpkgs.nix
, githubPath ? "default"
}:

(import ./. {dev = true; inherit githubPath;}).env
