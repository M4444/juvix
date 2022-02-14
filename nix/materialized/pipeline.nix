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
      identifier = { name = "pipeline"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2021 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/Pipeline#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.org" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."context" or (errorHandler.buildDepError "context"))
          (hsPkgs."core" or (errorHandler.buildDepError "core"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
          ];
        buildable = true;
        modules = [ "Paths_pipeline" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "pipeline-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."context" or (errorHandler.buildDepError "context"))
            (hsPkgs."core" or (errorHandler.buildDepError "core"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."wl-pprint-text" or (errorHandler.buildDepError "wl-pprint-text"))
            ];
          buildable = true;
          modules = [ "Paths_pipeline" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Pipeline;
    }) // { cabal-generator = "hpack"; }