# Building with Nix

The Nix integration uses [haskell.nix by
IOHK](https://input-output-hk.github.io/haskell.nix/index.html). It is
advisable to setup IOHK's binary caches in order to avoid compiling a lot of
stuff unnecessarily. [Check out this guide on how to do
that](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html).

You can speed up the evaluation of the nix expressions by materializing the
generated expressions

```bash
nix-build -A project.stack-nix.passthru.updateMaterialized --no-out-link | bash
```

Remember to re-run the above after changing any dependencies. Check
[here](https://input-output-hk.github.io/haskell.nix/tutorials/materialization.html)
for more details about materialization.


The `default.nix` actually evaluates to an attrset of:

- `project`: the main `haskell.nix` project generated via `stackProject'`. All
  other attributes below are just conveniences derived from this. Refer to the
  [haskell.nix
  reference](https://input-output-hk.github.io/haskell.nix/reference/library.html#haskell-package-description)
  for the full explanation of the contents and structure of this value.
- `local`: subset of Haskell packages built from local sources (per the
  `stack.yaml`). It is a subset of `project.hsPkgs`.
- `exes`: executables of local Haskell packages.
- `tests`: test suites of local Haskell packages.
- `static`: basically the same as `project`, but everything cross-compiled for
  `x86_64-unknown-linux-musl` and statically linked. Shorthand for
  `project.pkgsCross.musl64`.
- `static.local`: same as `local` but cross-compiled for
  `x86_64-unknown-linux-musl` and statically linked. Subset of
  `static.project.hsPkgs`.

----

Some other useful things you can find in `project` (all of these come from the
`haskell.nix` infrastructure):

- `project.pkgs.haskell-nix.haskellLib`: attrset of useful functions for
  working with haskell package sets. There's a
  [reference](https://input-output-hk.github.io/haskell.nix/reference/library.html#haskelllib)
  but it's far from complete (check the [source
  code](https://github.com/input-output-hk/haskell.nix/blob/master/lib/default.nix)
  for everything).

## Using `nix-build`

You must choose what you want to build by giving an attribute path to
`nix-build`. A few examples:

Build the `juvix` executable:

```bash
nix-build -A project.hsPkgs.juvix.components.exes.juvix
```

You may also use the `getComponent` utility function. The syntax corresponds to
that of Stack (`<package>:<component>:<name>`):

```bash
# Same as "-A project.hsPkgs.juvix.components.exes.juvix"
nix-build -E '(import ./. {}).project.getComponent "juvix:exe:juvix"'
```

Build statically linked version of `juvix-server`:

```bash
nix-build -A static.local.http.components.exes.juvix-server
```

Build all executables (from local Haskell packages):

```bash
# Also "tests" and "benchmarks" are available
nix-build -A exes
```

## Using `nix repl`

The Nix repl is very convenient for exploring nix expressions and building
ad-hoc derivations:

```
nix repl
nix-repl> :a import ./default.nix {}
nix-repl> :b project.getComponent "http:exe:juvix-server"
nix-repl> :b static.project.getComponent "http:exe:juvix-server"
```

## Using `nix-shell`

The default `shell.nix` shell enables developing of the haskell packages with
`ghci` or `cabal v2-build` (but not `stack`). It uses `shellFor` from the
`haskell.nix` infrastructure. See
<https://input-output-hk.github.io/haskell.nix/tutorials/development.html> for
details.

You can also use it to start a local Hoogle server which also serves the
haddock for all local packages and their dependencies:

```bash
nix-shell --run "hoogle server --local"
```

## Nix hashes for git sources

It's currently necessary to augment the stack configuration with nix hashes of
git dependencies, as explained in
<https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes.html>

The hashes live in file <./nix/stack-sha256map.nix> and must be kept in sync
with git sources in the stack configuration You can generate the hashes with a
prefetcher. This works for git sources:

```bash
nix-shell -p nix-prefetch-git --run 'nix-prefetch-git <url> <revision>'
```

## Nix sources

[Niv](https://github.com/nmattia/niv) is used to pin external nix sources (such
as `haskell.nix`). It's a good idea to update the sources every once in a
while:

```bash
nix-shell -p niv --run 'niv update'
```
