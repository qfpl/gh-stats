# `gh-stats`

Downloads, stores, and serves GitHub statistics for an organisation. Statistics include stars,
forks, views, clones, referrers, and popular paths. A detailed breakdown of views and clones by day
for each repo is also stored when updating the database.

## CSV

Output a summary as CSV to stdout. Includes top level statistics per repository.

```
gh-stats csv (-o|--org ORGANISATION) (-t|--token TOKEN)

$ gh-stats csv -o qfpl -t MyGitHubApiToken
```

## Update

Update a SQLite database with detailed traffic statistics for each repository under the
organisation. Failures kill the whole update, but any data already in the database should stay
there. Ideally any failures would get collected up and reported at the end without tanking the whole
update.

```
gh-stats update (-o|--org ORGANISATION) (-d|--db-path DB_PATH) (-t|--token TOKEN)

$ gh-stats update -o qfpl -d qfpl.sqlite -t MyGitHubApiToken
```

## Serve

Serve a SQLite database as some very basic HTML pages at `http://localhost:8000/repos`. Currently
the top level summary is all that is available. Configuring the port will get added soon.

```
gh-stats serve (-d|--db-path DB_PATH)

$ gh-stats serve -d qfpl.sqlite
```

## Building

I've only built this project with `nix`, however regular cabal should work fine. The bounds are very
restrictive at the moment, as they're based on the version of `nixpkgs` that is used by default to
build the project.

### Nix

If you have `nix` installed, then a `nix-build` in the root of the project should get you a binary
at `result/bin/gh-stats`. `nixpkgs` is pinned, and overlays take care of putting all the right
versions together for you.

If you want to run your own cabal commands while developing, then the following should do it.

```
$ nix-shell --run 'cabal v2-build'
```
