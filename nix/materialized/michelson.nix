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
      identifier = { name = "michelson"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2020 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/Michelson#readme>";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."core" or (errorHandler.buildDepError "core"))
          (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."morley" or (errorHandler.buildDepError "morley"))
          (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
          ];
        buildable = true;
        modules = [ "Paths_michelson" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "michelson-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."core" or (errorHandler.buildDepError "core"))
            (hsPkgs."data-structures" or (errorHandler.buildDepError "data-structures"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."michelson" or (errorHandler.buildDepError "michelson"))
            (hsPkgs."morley" or (errorHandler.buildDepError "morley"))
            (hsPkgs."pipeline" or (errorHandler.buildDepError "pipeline"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
            ];
          buildable = true;
          modules = [ "Paths_michelson" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Backends/Michelson;
    }) // { cabal-generator = "hpack"; }