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
      identifier = { name = "standard-library"; version = "0.1.0.0"; };
      license = "GPL-3.0-only";
      copyright = "2020 Heliax AG";
      maintainer = "Heliax AG <hello@heliax.dev>";
      author = "Heliax AG <hello@heliax.dev>";
      homepage = "https://github.com/heliaxdev/juvix#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/StandardLibrary#readme>";
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
          (hsPkgs."capability" or (errorHandler.buildDepError "capability"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."elliptic-curve" or (errorHandler.buildDepError "elliptic-curve"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."galois-field" or (errorHandler.buildDepError "galois-field"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."pretty-compact" or (errorHandler.buildDepError "pretty-compact"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-silver" or (errorHandler.buildDepError "tasty-silver"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."wreq" or (errorHandler.buildDepError "wreq"))
          ];
        buildable = true;
        modules = [ "Paths_standard_library" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "fetch-libs" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."capability" or (errorHandler.buildDepError "capability"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."elliptic-curve" or (errorHandler.buildDepError "elliptic-curve"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."galois-field" or (errorHandler.buildDepError "galois-field"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pretty-compact" or (errorHandler.buildDepError "pretty-compact"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-silver" or (errorHandler.buildDepError "tasty-silver"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."wreq" or (errorHandler.buildDepError "wreq"))
            ];
          buildable = true;
          modules = [ "Paths_standard_library" ];
          hsSourceDirs = [ "app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.incomplete-error) "";
          };
        };
      tests = {
        "standard-library-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."capability" or (errorHandler.buildDepError "capability"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."elliptic-curve" or (errorHandler.buildDepError "elliptic-curve"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."galois-field" or (errorHandler.buildDepError "galois-field"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pretty-compact" or (errorHandler.buildDepError "pretty-compact"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."standard-library" or (errorHandler.buildDepError "standard-library"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-silver" or (errorHandler.buildDepError "tasty-silver"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."wreq" or (errorHandler.buildDepError "wreq"))
            ];
          buildable = true;
          modules = [ "Paths_standard_library" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ././library/StandardLibrary;
    }) // { cabal-generator = "hpack"; }