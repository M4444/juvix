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
      identifier = { name = "data-structures"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2020 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/Juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/DataStructures#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.org" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."context" or (errorHandler.buildDepError "context"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
          ];
        buildable = true;
        modules = [ "Paths_data_structures" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "data-structures-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."context" or (errorHandler.buildDepError "context"))
            (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."list-t" or (errorHandler.buildDepError "list-t"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            ];
          buildable = true;
          modules = [ "Paths_data_structures" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Test/DataStructures;
    }) // { cabal-generator = "hpack"; }