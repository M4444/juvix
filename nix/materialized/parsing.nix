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
      identifier = { name = "parsing"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2020 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "Juvix Frontend Datastructure";
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
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          ];
        buildable = true;
        modules = [ "Paths_parsing" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "parsing-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."parsing" or (errorHandler.buildDepError "parsing"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            ];
          buildable = true;
          modules = [ "Paths_parsing" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/Parsing;
    }) // { cabal-generator = "hpack"; }