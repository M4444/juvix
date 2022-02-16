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
    flags = { incomplete-error = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "juvix"; version = "0.1.1.19"; };
      license = "GPL-3.0-only";
      copyright = "2018 Christopher Goes, 2018-2019 Cryptium Labs, 2019- Heliax AG";
      maintainer = "cwgoes@heliax.dev, jeremy@heliax.dev, marty@heliax.dev, andy@heliax.dev";
      author = "Heliax AG";
      homepage = "https://github.com/anoma/juvix#readme";
      url = "";
      synopsis = "Juvix smart contract language compiler, debugging toolkit, & stateful deployment system";
      description = "Please see the README on GitHub at <https://github.com/anoma/juvix#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."core" or (errorHandler.buildDepError "core"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."easy" or (errorHandler.buildDepError "easy"))
          (hsPkgs."llvm" or (errorHandler.buildDepError "llvm"))
          (hsPkgs."michelson" or (errorHandler.buildDepError "michelson"))
          (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
          (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
          (hsPkgs."witch" or (errorHandler.buildDepError "witch"))
          ];
        buildable = true;
        modules = [ "Paths_juvix" "Juvix" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "juvix" = {
          depends = [
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."core" or (errorHandler.buildDepError "core"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."easy" or (errorHandler.buildDepError "easy"))
            (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
            (hsPkgs."juvix" or (errorHandler.buildDepError "juvix"))
            (hsPkgs."llvm" or (errorHandler.buildDepError "llvm"))
            (hsPkgs."michelson" or (errorHandler.buildDepError "michelson"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
            (hsPkgs."witch" or (errorHandler.buildDepError "witch"))
            ];
          buildable = true;
          modules = [ "Paths_juvix" ];
          hsSourceDirs = [ "app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.incomplete-error) "";
          };
        };
      benchmarks = {
        "juvix-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."core" or (errorHandler.buildDepError "core"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."easy" or (errorHandler.buildDepError "easy"))
            (hsPkgs."juvix" or (errorHandler.buildDepError "juvix"))
            (hsPkgs."llvm" or (errorHandler.buildDepError "llvm"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."michelson" or (errorHandler.buildDepError "michelson"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
            (hsPkgs."witch" or (errorHandler.buildDepError "witch"))
            ];
          buildable = true;
          modules = [ "Paths_juvix" ];
          hsSourceDirs = [ "bench" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }) // {
    cabal-generator = "hpack";
    }