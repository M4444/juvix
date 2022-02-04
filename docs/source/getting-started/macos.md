# Installation notes for macOS

These notes are a work-in-progress set of steps for building the Juvix project on macOS.

## llvm-hs Setup Instructions

Juvix uses the [llvm-hs library](https://hackage.haskell.org/package/llvm-hs) to provide bindings to [LLVM](http://llvm.org/docs/LangRef.html). The current version of `llvm-hs` on Hackage provides bindings to llvm version 9 and therefore your system requires an installation of LLVM 9.

The [llvm@9 homebrew formulae](https://formulae.brew.sh/formula/llvm@9) is not setup in the way that `llvm-hs` expects. So you'll need to build LLVM from source.

### Building LLVM 9 from source

Follow the instructions for building LLVM from source found [on the `llvm-hs` README](https://github.com/llvm-hs/llvm-hs/tree/llvm-9#building-from-source).

1. Use the [LLVM 9.0.1 source](https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/llvm-9.0.1.src.tar.xz) instead of the link to version 9.0.0 that it provides.
2. Set the `$INSTALL_PREFIX` to `/usr/local/opt/llvm-9-src` to avoid conflicts with other installations of LLVM
3. It's important to follow all of the steps, including step 5 to apply post install fixes
4. The `llvm-hs` build expects `llvm-config-9` to be on `$PATH`. So create a symbolic link `$INSTALL_PREFIX/bin/llvm-config-9` to `$INSTALL_PREFIX/bin/llvm-config` using:`ln -s $INSTALL_PREFIX/bin/llvm-config-9 $INSTALL_PREFIX/bin/llvm-config`.

### Building llvm-hs

The llvm-hs library will be built when you run `stack build` in the Juvix project. As part of this build GHC will build some C++ files that need to link with LLVM 9.0.1.

Before running `stack build` for the first time in the Juvix project follow these instructions to setup the environment.

1. Add the following option to your `~/.stack/config.yaml`

```
ghc-options:
  llvm-hs: -optcxx=-std=c++11 -optcxx=-lstdc++ -optcxx=-fno-rtti -optcxx=-Wno-init-list-lifetime -optcxx=-Wno-stringop-overflow
```

2. Add `$INSTALL_PREFIX/bin` (using the `$INSTALL_PREFIX` you used when building LLVM 9 from source) to the beginning of your `$PATH` environment variable in the shell where you're going to run `stack build`.

```
export PATH=$INSTALL_PREFIX/bin:$PATH
```
