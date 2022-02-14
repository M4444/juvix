{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "llvm"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2021 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "LLVM backend for the Juvix programming language.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."core" or (errorHandler.buildDepError "core"))
          (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."llvm-hs" or (errorHandler.buildDepError "llvm-hs"))
          (hsPkgs."llvm-hs-pretty" or (errorHandler.buildDepError "llvm-hs-pretty"))
          (hsPkgs."llvm-hs-pure" or (errorHandler.buildDepError "llvm-hs-pure"))
          (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [ "Paths_llvm" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "llvm-test" = {
          depends = [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."core" or (errorHandler.buildDepError "core"))
            (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."llvm" or (errorHandler.buildDepError "llvm"))
            (hsPkgs."llvm-hs" or (errorHandler.buildDepError "llvm-hs"))
            (hsPkgs."llvm-hs-pretty" or (errorHandler.buildDepError "llvm-hs-pretty"))
            (hsPkgs."llvm-hs-pure" or (errorHandler.buildDepError "llvm-hs-pure"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [ "Paths_llvm" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Backends/llvm;
    }) // { cabal-generator = "hpack"; }