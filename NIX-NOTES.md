# Building with Nix

The Nix integration uses [haskell.nix by
IOHK](https://input-output-hk.github.io/haskell.nix/index.html). It is
advisable to setup IOHK's binary caches in order to avoid compiling a lot of
stuff unnecessarily. [Check out this guide on how to do
that](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html).

## Using `nix` and flakes

Flakes are a new system for Nix. It makes many things easier but also
increases already high learning curve.

Haskell.nix uses IFD (import-from-derivation) quite a bit. With Flakes, IFD is
not even allowed by default, so it's a bit painful at times. You may need to
add `--option allow-import-from-derivation true` to some subcommands of the new
`nix` interface.

Couple of commands to get you started:

- `nix flake check`: check the flake for potential problems.
- `nix flake show [<installable>]`: show all output paths from (`installable`
  defaults to `.`).
- `nix build <installable>` build some flake outputs (similar to `nix-build`).
  For exmple, `nix build .#juvix:exe:juvix` would build the juvix executable and
  link it as `./result` in the current directory. The installable supports lots
  of formats (including remote sources). See `nix flake --help` for details.
- `nix develop`: launch (the default) development shell (similar to `nix-shell`).
- `nix update`: update all pins on nix dependencies (updates `flake.lock`).

## Using `nix-build` and `nix-shell`

`default.nix` and `shell.nix` are setup to use `flake-compat` to call the flake.

- `nix-build` builds `defaultPackage` by default.
- `nix-shell` loads `devShell` by default.
- All flake outputs are available as attributes, but you need to use the full
  paths for them
  i.e. `nix-build -A packages.x86_64-linux."http:exe:juvix-server"`, whereas
  flake attributes can fill in the blanks automatically: `nix build
  .#http:exe:juvix-server`.

## About `haskell.nix`

### Nix hashes for git sources

*TODO: update this section*

It's currently necessary to augment the stack configuration with nix hashes of
git dependencies, as explained in
<https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes.html>

The hashes live in file <./nix/stack-sha256map.nix> and must be kept in sync
with git sources in the stack configuration You can generate the hashes with a
prefetcher. This works for git sources:

```bash
nix-shell -p nix-prefetch-git --run 'nix-prefetch-git <url> <revision>'
```

### Using the development shell

The development shell (`devShell`) enables developing the haskell packages with
`ghci` or `cabal v2-build` (but not `stack`). It uses `shellFor` from the
`haskell.nix` infrastructure. See
<https://input-output-hk.github.io/haskell.nix/tutorials/development.html> for
details.

```bash
# Enter the shell
nix develop

# Directly start a hoogle server that runs in the shell
nix develop -c hoogle server --local

# Enter the shell environment for a single component
nix develop .#juvix:lib:juvix
```

You may also use `nix-shell` (only arguments are different).

### Materialization

You can speed up the evaluation of nix expressions by materializing the
generated expressions. This is currently not enabled. With flakes, this only
works if the materialized files are tracked by git. To enable materialization
you can do:

```bash
nix run .#generateMaterialized nix/materialized
git add nix/materialized
```

Remember to rematerialize after changing any dependencies. Check
[here](https://input-output-hk.github.io/haskell.nix/tutorials/materialization.html)
for more details about materialization.

### Hacking

You can use `nix repl` to efficiently inspect stuff:

```
nix repl
nix-repl> :lf .
nix-repl> project = legacyPackages.x86_64-linux.project
nix-repl> :b project.getComponent "juvix:exe:juvix"
```

`project` above is the main `haskell.nix` project generated via
`stackProject'`. See `project.nix` and the [haskell.nix
reference](https://input-output-hk.github.io/haskell.nix/reference/library.html#haskell-package-description)

Some useful things you can find in `project` (all of these come from the
`haskell.nix` infrastructure):

- `project.pkgs.haskell-nix.haskellLib`: attrset of useful functions for
  working with haskell package sets. There's a
  [reference](https://input-output-hk.github.io/haskell.nix/reference/library.html#haskelllib)
  but it's far from complete (check the [source
  code](https://github.com/input-output-hk/haskell.nix/blob/master/lib/default.nix)
  for everything).
