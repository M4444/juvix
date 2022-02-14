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
      identifier = { name = "berlin-pipeline"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2021 Author name here";
      maintainer = "example@example.com";
      author = "Author name here";
      homepage = "https://github.com/anoma/Juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/BerlinPipeline#readme>";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."context" or (errorHandler.buildDepError "context"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ];
        buildable = true;
        modules = [ "Paths_berlin_pipeline" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "berlin-pipeline-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."berlin-pipeline" or (errorHandler.buildDepError "berlin-pipeline"))
            (hsPkgs."context" or (errorHandler.buildDepError "context"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sexp" or (errorHandler.buildDepError "sexp"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            ];
          buildable = true;
          modules = [ "Paths_berlin_pipeline" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/BerlinPipeline;
    }) // { cabal-generator = "hpack"; }