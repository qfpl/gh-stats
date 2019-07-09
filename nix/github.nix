builtins.fetchGit {
  name = "qfpl-github-fork";
  url = https://github.com/qfpl/github/;
  # `git ls-remote https://github.com/qfpl/github traffic-nix`
  ref = "traffic-nix";
  rev = "a2e43623c4c1b832aa56f0c44f0fa04906724ccd";
}
