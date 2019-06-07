builtins.fetchGit {
  name = "qfpl-github-fork";
  url = https://github.com/qfpl/github/;
  # `git ls-remote https://github.com/qfpl/github traffic-nix`
  ref = "traffic-nix";
  rev = "3a631793289aa50091e19330d229fb4f040457fe";
}
