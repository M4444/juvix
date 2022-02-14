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
      identifier = { name = "witch"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 Author name here";
      maintainer = "example@example.com";
      author = "Author name here";
      homepage = "https://github.com/githubuser/Witch#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/Witch#readme>";
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
          (hsPkgs."context" or (errorHandler.buildDepError "context"))
          (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
          (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
          ];
        buildable = true;
        modules = [ "Paths_witch" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "witch-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."context" or (errorHandler.buildDepError "context"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."translate" or (errorHandler.buildDepError "translate"))
            (hsPkgs."witch" or (errorHandler.buildDepError "witch"))
            ];
          buildable = true;
          modules = [ "Paths_witch" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Witch;
    }) // { cabal-generator = "hpack"; }