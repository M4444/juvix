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
      identifier = { name = "translate"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2020 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/Translate#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."context" or (errorHandler.buildDepError "context"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
          (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [ "Paths_translate" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "translate-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."context" or (errorHandler.buildDepError "context"))
            (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [ "Paths_translate" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Translate;
    }) // { cabal-generator = "hpack"; }