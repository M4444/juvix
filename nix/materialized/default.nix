{
  extras = hackage:
    {
      packages = {
        "servant-static-th" = (((hackage.servant-static-th)."1.0.0.0").revisions).default;
        "llvm-hs-pure" = (((hackage.llvm-hs-pure)."9.0.0").revisions).default;
        "llvm-hs-pretty" = (((hackage.llvm-hs-pretty)."0.9.0.0").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hex-text" = (((hackage.hex-text)."0.1.0.0").revisions).default;
        "show-type" = (((hackage.show-type)."0.1.1").revisions).default;
        "cryptonite" = (((hackage.cryptonite)."0.27").revisions).default;
        "uncaught-exception" = (((hackage.uncaught-exception)."0.1.0").revisions).default;
        "tasty-hunit-compat" = (((hackage.tasty-hunit-compat)."0.2.0.1").revisions).default;
        "capability" = (((hackage.capability)."0.4.0.0").revisions)."d86d85a1691ef0165c77c47ea72eac75c99d21fb82947efe8b2f758991cf1837";
        "aeson-options" = (((hackage.aeson-options)."0.1.0").revisions)."2d0c25afbb2d038bd5b57de8d042e319ea1a5ec7d7b92810d8a0cf0777882b6a";
        "unexceptionalio" = (((hackage.unexceptionalio)."0.5.0").revisions)."ad0b2d4d1f62a3e24cdb80360eea42ab3f0a0559af42aba19b5cf373378913ce";
        "witherable" = (((hackage.witherable)."0.3.5").revisions)."6590a15735b50ac14dcc138d4265ff1585d5f3e9d3047d5ebc5abf4cd5f50084";
        "witherable-class" = (((hackage.witherable-class)."0").revisions)."91f05518f9f4af5b02424f13ee7dcdab5d6618e01346aa2f388a72ff93e2e501";
        "fgl-visualize" = (((hackage.fgl-visualize)."0.1.0.1").revisions)."e682066053a6e75478a08fd6822dd0143a3b8ea23244bdb01dd389a266447c5e";
        "tasty" = (((hackage.tasty)."1.4.1").revisions)."69e90e965543faf0fc2c8e486d6c1d8cf81fd108e2c4541234c41490f392f94f";
        "base16-bytestring" = (((hackage.base16-bytestring)."0.1.1.7").revisions)."0021256a9628971c08da95cb8f4d0d72192f3bb8a7b30b55c080562d17c43dd3";
        "named" = (((hackage.named)."0.3.0.1").revisions)."2975d50c9c5d88095026ffc1303d2d9be52e5f588a8f8bcb7003a04b79f10a06";
        "with-utf8" = (((hackage.with-utf8)."1.0.2.2").revisions)."42eed140390b3e93d9482b084d1d0150e8774667f39c33bd47e84815751fad09";
        "stm-containers" = (((hackage.stm-containers)."1.2").revisions)."a887f2e7692b7cf20e0b081e2d66e21076e2bd4b57016ec59c484edfa2d29397";
        "stm-hamt" = (((hackage.stm-hamt)."1.2.0.6").revisions)."fba86ccb4b45c5706c19b0e1315ba63dcac3b5d71de945ec001ba921fae80061";
        "constraints-extras" = (((hackage.constraints-extras)."0.3.0.2").revisions)."bf6884be65958e9188ae3c9e5547abfd6d201df021bff8a4704c2c4fe1e1ae5b";
        "dependent-sum" = (((hackage.dependent-sum)."0.7.1.0").revisions)."5599aa89637db434431b1dd3fa7c34bc3d565ee44f0519bfbc877be1927c2531";
        "dependent-sum-template" = (((hackage.dependent-sum-template)."0.1.0.3").revisions)."0bbbacdfbd3abf2a15aaf0cf2c27e5bdd159b519441fec39e1e6f2f54424adde";
        "hashing" = (((hackage.hashing)."0.1.0.1").revisions)."98861f16791946cdf28e3c7a6ee9ac8b72d546d6e33c569c7087ef18253294e7";
        "monoidal-containers" = (((hackage.monoidal-containers)."0.6.0.1").revisions)."7d776942659eb4d70d8b8da5d734396374a6eda8b4622df9e61e26b24e9c8e40";
        juvix = ./juvix.nix;
        standard-library = ./standard-library.nix;
        parsing = ./parsing.nix;
        core = ./core.nix;
        pipeline = ./pipeline.nix;
        berlin-pipeline = ./berlin-pipeline.nix;
        translate = ./translate.nix;
        michelson = ./michelson.nix;
        easy = ./easy.nix;
        http = ./http.nix;
        llvm = ./llvm.nix;
        witch = ./witch.nix;
        context = ./context.nix;
        sexp = ./sexp.nix;
        data-structures = ./data-structures.nix;
        pretty-compact = ./.stack-to-nix.cache.0;
        extensible-data = ./.stack-to-nix.cache.1;
        sr-extra = ./.stack-to-nix.cache.2;
        tasty-silver = ./.stack-to-nix.cache.3;
        morley = ./.stack-to-nix.cache.4;
        morley-prelude = ./.stack-to-nix.cache.5;
        caps = ./.stack-to-nix.cache.6;
        base-noprelude = ./.stack-to-nix.cache.7;
        jsonschema-gen = ./.stack-to-nix.cache.8;
        elliptic-curve = ./.stack-to-nix.cache.9;
        pairing = ./.stack-to-nix.cache.10;
        galois-field = ./.stack-to-nix.cache.11;
        };
      };
  resolver = "lts-18.16";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }