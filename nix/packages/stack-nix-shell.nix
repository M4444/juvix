# Better (faster) shell for use with stack's nix integration.
#
# Usage:
#    nix develop -c stack ghci
#
# Note, that ghcVersion is intended to be overridable for ease of use with stack nix
# integration (where stack passes ghcVersion as a parameter to the nix
# shell expression).

{ lib
, ghcVersion
, stdenv
, writeShellScriptBin
, runCommand
, haskell
, gcc
, llvm_9
, zlib
, curl
, time
, ldb
, git
, glibcLocales
}:

let
  stackWrap = writeShellScriptBin "stack" ''
    STACK=$(PATH=$(echo $PATH | sed 's,/nix/[^:]*:,,g') type -p stack)
    exec "$STACK" $STACK_IN_NIX_EXTRA_ARGS --internal-re-exec-version="$("$STACK" --numeric-version)" "$@"
  '' // { version = "0.0.0"; };
  ghc = haskell.compiler.${ghcVersion};
  inputs = [ stackWrap ghc gcc llvm_9 zlib curl time ldb git ];
  libPath = lib.makeLibraryPath inputs;
  stackExtraArgs = lib.concatMap
    (pkg:
      [
        ''--extra-lib-dirs=${lib.getLib pkg}/lib''
        ''--extra-include-dirs=${lib.getDev pkg}/include''
      ]
    )
    inputs;

in
runCommand "stack-nix-shell"
{
  buildInputs = lib.optional stdenv.isLinux glibcLocales ++ inputs;
  LD_LIBRARY_PATH = libPath;
  STACK_IN_NIX_EXTRA_ARGS = stackExtraArgs;
  STACK_PLATFORM_VARIANT = "nix";
  STACK_IN_NIX_SHELL = 1;
} ""
