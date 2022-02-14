# Nix - General Beginners Guide

## Getting started

1.  Install Nix from <https://nixos.org/download.html>. Make sure to get
    version 2.4 or later.

2.  Enable the experimental features `flakes` and `nix-command`:

    ```bash
    mkdir -p ~/.config/nix
    tee >> ~/.config/nix/nix.conf <<'END'
    experimental-features = nix-command flakes
    allow-import-from-derivation = true
    END
    ```

3.  Configure additional binary caches. Binary caches need to be configured in
    the system-wide configuration `/etc/nix/nix.conf`. (Unless you have
    single-user Nix installation or have set yourself as trusted user in the
    system-wide config). If you have `sudo` permissions, simply run:

    ```bash
    sudo tee -a /etc/nix/nix.conf <<'END'
    extra-substituters = https://hydra.iohk.io
    extra-substituters = s3://heliaxdev-nixcache?region=eu-west-1
    extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    extra-trusted-public-keys = heliaxdev-nixcache?region=eu-west-1:GgmKSs1JLZWfQFWpGi+3cy7kb7bGZ19UBOHgaXdvuQg=
    END
    ```

    One of these [is from IOHK for their `haskell.nix` framework](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html),
    and the other is provided by Heliax.

4.  Now we are all set to use flakes. Let's explore a bit to get a feeling.
    Remember how we enabled the other feature called `nix-command`? That means
    the new Nix command called (literally) `nix`. It's currently the only Nix
    command that's adequetely integrated with flakes. Also much of the
    functionality of the older `nix-build`, `nix-shell`, `nix-store`,
    `nix-instantiate`, etc. is implemented in `nix` in some slightly different
    ways. For example, here's how you search for `nyancat` in the (latest
    version of) the nixpkgs flake:

    ```
    > nix search nixpkgs nyancat

    * legacyPackages.x86_64-linux.nyancat (1.5.2)
      Nyancat in your terminal, rendered through ANSI escape sequences
    ```

    This is how you launch nyancat:

    ```
    > nix run nixpkgs#nyancat
    ```

    But what if the version of nyancat we got was broken? No problem, we can
    summon a ghost of christmas past just as easily:

    ```
    > nix run nixpkgs/nixos-20.09#nyancat
    ```

    Now we want to make sure, that we are able to realize this exact good
    nyancat long into the future. So we look up the locked URL of the flake:

    ```
    > nix flake metadata nixpkgs/nixos-20.09

    Resolved URL:  github:NixOS/nixpkgs/nixos-20.09
    Locked URL:    github:NixOS/nixpkgs/1c1f5649bb9c1b0d98637c8c365228f57126f361
    Description:   A collection of packages for the Nix package manager
    Path:          /nix/store/w5i4gmcbscmwmmzp6k1gq0p7ixdplrrx-source
    Revision:      1c1f5649bb9c1b0d98637c8c365228f57126f361
    Last modified: 2021-10-27 17:53:25
    Inputs:
    ```

    And there we have it (the `github:NixOS/` prefix is default and can be
    omitted):

    ```
    alias nyancat='nix run nixpkgs/1c1f5649bb9c1b0d98637c8c365228f57126f361#nyancat'
    ```

Ok, simple enough:

- We use `nix flake metadata` to query basic information about a flake.
- `nix search` searches a flake for attributes that match a query.
- `nix run` runs executables from flakes.

## Flake metadata and inputs

This repo is a flake, because there's a `flake.nix` (and `flake.lock`) file
there. Local as well as remote flakes can be referenced with "flakerefs"
(explained in `nix flake --help`):

```
> nix flake metadata github:anoma/juvix/8d36907d65bab9a039325cf9609f7e6047a2f2a9

Resolved URL:  github:anoma/juvix/8d36907d65bab9a039325cf9609f7e6047a2f2a9
Locked URL:    github:anoma/juvix/8d36907d65bab9a039325cf9609f7e6047a2f2a9
Description:   Juvix
Path:          /nix/store/y768rsj21z8hsnh5qljihx54f716yjc4-source
Revision:      8d36907d65bab9a039325cf9609f7e6047a2f2a9
Last modified: 2021-12-21 11:41:44
Inputs:
├───flake-compat: github:edolstra/flake-compat/12c64ca55c1014cdc1b16ed5a804aa8576601ff2
├───flake-utils: github:numtide/flake-utils/74f7e4319258e287b0f9cb95426c9853b282730b
├───haskellNix: github:input-output-hk/haskell.nix/b5f39bffd149324fd06f4122654bfd28223dc250
│   ├───HTTP: github:phadej/HTTP/9bc0996d412fef1787449d841277ef663ad9a915
│   ├───cabal-32: github:haskell/cabal/94aaa8e4720081f9c75497e2735b90f6a819b08e
│   ├───cabal-34: github:haskell/cabal/b086c1995cdd616fc8d91f46a21e905cc50a1049
│   ├───cardano-shell: github:input-output-hk/cardano-shell/9392c75087cb9a3d453998f4230930dea3a95725
│   ├───flake-utils: github:numtide/flake-utils/f7e004a55b120c02ecb6219596820fcd32ca8772
│   ├───ghc-8.6.5-iohk: github:input-output-hk/ghc/95713a6ecce4551240da7c96b6176f980af75cae
│   ├───hackage: github:input-output-hk/hackage.nix/4b8d0337efdddf49749165286c8db8c93a165f8f
│   ├───hpc-coveralls: github:sevanspowell/hpc-coveralls/14df0f7d229f4cd2e79f8eabb1a740097fdfa430
│   ├───nix-tools: github:input-output-hk/nix-tools/ed5bd7215292deba55d6ab7a4e8c21f8b1564dda
│   ├───nixpkgs follows input 'haskellNix/nixpkgs-2111'
│   ├───nixpkgs-2003: github:NixOS/nixpkgs/1db42b7fe3878f3f5f7a4f2dc210772fd080e205
│   ├───nixpkgs-2105: github:NixOS/nixpkgs/499ca2a9f6463ce119e40361f4329afa921a1d13
│   ├───nixpkgs-2111: github:NixOS/nixpkgs/453bcb8380fd1777348245b3c44ce2a2b93b2e2d
│   ├───nixpkgs-unstable: github:NixOS/nixpkgs/e6df26a654b7fdd59a068c57001eab5736b1363c
│   ├───old-ghc-nix: github:angerman/old-ghc-nix/af48a7a7353e418119b6dfe3cd1463a657f342b8
│   └───stackage: github:input-output-hk/stackage.nix/a8e658e6a4d02cecc2073e2b3d661dd3bb42164b
├───nix-bundle: github:matthewbauer/nix-bundle/223f4ffc4179aa318c34dc873a08cb00090db829
│   └───nixpkgs follows input 'nixpkgs'
└───nixpkgs follows input 'haskellNix/nixpkgs-unstable'
```

We can also point Nix at a local checkout of the repository: `nix flake show .`
(the `.` can even be omitted). This is often convenient, because Nix can pick
up your uncommitted, unstaged changes from the worktree.

Above we see other flakes that ours uses as inputs. Inputs and outputs are all
defined in [flake.nix](./flake.nix). In fact, we see the whole dependency tree:
`haskell.nix` has a bunch inputs we are also seeing (and can import ourselves;
we're even allowed to override them).

## Listing outputs (evaluating the flake)

So whatabout our outputs then? Surely we produce something interesting with all
those inputs! Let's see:

```
> nix flake show github:anoma/juvix/8d36907d65bab9a039325cf9609f7e6047a2f2a9

[....]
error: a 'x86_64-darwin' with features {} is required to build '/nix/store/mdyn9laf40f06kh6h28g50k28abp1gjr-nix-tools-plan-to-nix-pkgs.drv', but I am a 'x86_64-linux' with features {benchmark, big-parallel, kvm, nixos-test, recursive-nix}
(use '--show-trace' to show detailed location information)
```

*Oh no! What? Why? I'm on Linux, there's no reason to require Mac software!*

Long story short: flakes are evaluated in "pure" mode by default.

*Okay, so?*

So, for a long time Nix has had these "impure" escape hatches, such as
`builtins.currentSystem`, which yields the system of the machine evaluating the
Nix expression. Flakes prohibit impure functionality like this by default (lack
of `builtins.currentSystem` is why you must be explicit about the system(s)
your outputs are for), and unfortunately not every package has been adapted
yet.

*Okay, cool, whatever. I still want to see the outputs though...*

Sadly, it seems that adding `--impure` is the only option with some commands
(`nix flake show` at least):

```
nix flake show  github:anoma/juvix/8d36907d65bab9a039325cf9609f7 --impure
```

(Make sure you have set `allow-import-from-derivation = true` in your
`nix.conf`. Alternatively you can set it for the command: `nix --option
allow-import-from-derivation true ...`)

It should pretty-print all output derivations in a tree format. All lead nodes
like `packages.x86_64-darwin."juvix:exe:juvix"` can be used as fragments for
the various `nix` commands, and you don't need to be exact - if you said
`nix build .#juvix:exe:juvix`, Nix will pick the right package for your system
automatically.

## Flake's structure

The schema of flakes is
[well-defined](https://nixos.wiki/wiki/Flakes#Flake_schema) (although not super
strictly enforced). Attributes in one group (`apps`, `packages`,
`devShell`, ...) don't always make sense with all `nix` subcommands:

- `apps`: things meant to be executed via `nix run`
- `packages`: normal packages (exported as flake outputs) for use with `nix
  build`, `nix profile`, `nix shell`, as dependencies, etc. etc.
- `legacyPackages`: nixpkgs packages (non-flake)
- `devShell`: shell environments for use with `nix develop`

## Small list of useful Flakes-enabled commands

- `nix flake check`: check the flake for potential problems.
- `nix flake show [<installable>]`: show all output paths from (`installable`
  defaults to `.`).
- `nix build <installable>` build some flake outputs (similar to `nix-build`).
  For example, `nix build .#juvix:exe:juvix` would build the juvix executable and
  link it as `./result` in the current directory. The installable supports lots
  of formats (including remote sources). See `nix flake --help` for details.
- `nix develop`: launch (the default) development shell (similar to `nix-shell`).
- `nix update`: update all pins on nix dependencies (updates `flake.lock`).

The `nix-build`, `nix-shell` and other older Nix commands hardly know about
flakes. The `default.nix` and `shell.nix` are setup to use `flake-compat` to
call the flake:

- `nix-build` builds `defaultPackage` by default.
- `nix-shell` loads `devShell` by default.
- All flake outputs are available as attributes, but you need to use the full
  paths for them
  i.e. `nix-build -A packages.x86_64-linux."http:exe:juvix-server"`, whereas
  flake attributes can fill in the blanks automatically: `nix build
  .#http:exe:juvix-server`.

## Exploring in the REPL

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


## Links and resources

- [Awesome Nix](https://nix-community.github.io/awesome-nix/)
- [Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language)
- [Nix manual](https://nixos.org/manual/nix/unstable/introduction.html)
- [The `nix flake` commands](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html)
- [Flakes on NixOS wiki](https://nixos.wiki/wiki/Flakes)
- [Blog post "Practical Nix Flakes" (Serokell)](https://serokell.io/blog/practical-nix-flakes)
- [Haskell.nix framework (from IOHK)](https://input-output-hk.github.io/haskell.nix/index.html)
