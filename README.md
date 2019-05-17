# Basic Github stats

Given an organisation name, dump the stars and forks for every repo under that organisation to `stdout` as a CSV.

This has only been tested with `cabal new-*` in a nix shell, but it should work fine without the shell.

## Building

```
$ nix-shell --run 'cabal new-build'
```

## Running

The `gh-stats` exe takes one argument --- the name of the organisation. It does no checking or processing on the arguments, because YOLO.

```
$ nix-shell --run 'cabal new-run gh-stats qfpl'
```
