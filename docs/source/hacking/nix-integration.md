# Nix Integration

## Recommended Settings

Options for Nix daemon / enabled by default:

```
experimental-features = nix-command flakes
trusted-users = root $USER
allow-import-from-derivation = true
extra-substituters = s3://heliaxdev-nixcache?region=eu-west-1 https://hydra.iohk.io
extra-trusted-public-keys = heliaxdev-nixcache?region=eu-west-1:GgmKSs1JLZWfQFWpGi+3cy7kb7bGZ19UBOHgaXdvuQg= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

- Replace `$USER` with your username.
- You can omit the `trusted-users` line if you do not intend to build via
  `stack`.
- It is recommended to enable IOHK's binary cache, because their `haskell.nix`
  infrastructure is used in Juvix.
- The Heliax-managed binary cache hosts some Juvix-specific paths. It's
  also recommended to enable.


## The Default Shell (`devShell`)

Many convenience commands are provided by the default dev shell. Launch it
using `nix develop`:

```bash
nix develop --impure
```

A menu screet pops up, which lists the commands available inside the shell:

```
🔨 Welcome to juvix-nix

[docs]

  docs-shell          - Build the Juvix documentation.

[general commands]

  menu                - prints this menu

[haskell]

  haskell-nix-shell   - haskell.nix dev shell
  hoogle-server       - Generate Hoogle index for a set of packages and launch the web interface.
  stack-nix-shell     - Faster shell to execute Nix-enabled Stack commands in.

[nix]

  update-materialized - Update (generate) `nix/stack-sha256` and `nix/materialized/`

```

For example, typing `hoogle-server` would launch a local Hoogle server (the database populated with all Juvix packages and dependendent packages.)

You can also launch the same shell commands from outside the shell:

```bash
nix develop -c stack-nix-shell
```

`stack-nix-shell` provides an environment for running nix-enabled `stack`
commands (`stack build`, etc.) without having to wait for stack to reload the
nix shell all the time.

### Using `direnv`

If you use [direnv](https://direnv.net/), then you can allow the `.envrc` in
to automatically load the development shell whenever you're inside the
repository.

## Packages and Other Outputs

- All Haskell packages are exported as `<package>:<component>:<name>`.
- Package `executables` contains executables from all Juvix packages in a
  single derivation. (Similar to Stack's default behavior when installing a
  multi-package project.)
- Package `juvix-docs` builds the Juvix docs. _Note: Haddock documentation is
  not included in this derivation._
- Packages `x86_64-unknown-linux-musl:<package>:<component>:<name>` are
  statically linked (MUSL) variants of the Haskell packages.

Examples:

```bash
# Build all executables for current system
nix build .#executables

# Build static juvix-server executable
nix build .#x86_64-unknown-linux-musl:http:exe:juvix-server

# Build juvix docs
nix build .#juvix-docs

# Build and install to user profile Juvix executables from remote flake
nix profile install github:anoma/juvix#executables
```

## Nix hashes for git sources

It's currently necessary to augment the stack configuration with nix hashes of
git dependencies, as explained in
<https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes.html>

The hashes live in file [nix/stack-sha256map.nix](./nix/stack-sha256map.nix) and must be kept in sync
with git sources in the stack configuration You can generate the hashes with a
prefetcher. This works for git sources:

```bash
nix run 'nixpkgs#nix-prefetch-git' -- <url> <rev>
```

## Materialization

You can speed up the evaluation of nix expressions by materializing the
generated expressions. This is currently not enabled. With flakes, this only
works if the materialized files are tracked by git. To enable materialization
you can do:

```bash
nix develop -c update-materialized  # option 1

nix run .#updateMaterialized        # option 2
```

Remembor to add and commit the materialized files. Keep them up to date in
the repository as a convenience to others.

Remember to rematerialize after changing any dependencies.

[Here](https://input-output-hk.github.io/haskell.nix/tutorials/materialization.html)
you can read more about the reasons behind materialization.
