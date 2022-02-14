final: prev:
let
  py = final.python39;
in
  with { inherit (py.pkgs) buildPythonPackage fetchPypi; };
rec {
  juvixDocsEnv = final.buildEnv {
    name = "juvix-docs-env";
    paths = [
      (py.withPackages (ps: with ps; [
        sphinx-autobuild
        sphinx_rtd_theme
        myst-parser
        sphinx-proof
        sphinxcontrib-tikz
        sphinxcontrib-bibtex
      ]))
      (with final.texlive; combine {
        inherit scheme-small latexmk collection-latexextra bussproofs cmll;
      })
    ];
  };

  juvix-docs = final.stdenv.mkDerivation {
    name = "juvix-docs";
    src = ../docs;
    nativeBuildInputs = [ juvixDocsEnv ];
    buildPhase = ''
      sphinx-build source build
      make latexpdf
    '';
    installPhase = ''
      mv build $out
    '';
  };

  sphinx-proof = buildPythonPackage rec {
    pname = "sphinx-proof";
    version = "0.1.3";
    src = fetchPypi rec {
      inherit pname version;
      sha256 = "sha256-25ONoeoYiA+A8Ol5y4lWBKSQvv8+jz3RlF1Jl0UbNzs=";
    };
    propagatedBuildInputs = with py.pkgs; [ sphinx-book-theme ];
  };

  pydata-sphinx-theme = buildPythonPackage rec {
    # XXX Build of this is bloated (builds its own auxilary nodejs
    # universe using network); using a pre-packaged wheel to avoid dealing
    # with that in nix.
    pname = "pydata-sphinx-theme";
    version = "0.8.0";
    src = ./pydata_sphinx_theme-0.8.0-py3-none-any.whl;
    format = "wheel";
    propagatedBuildInputs = with py.pkgs; [ sphinx beautifulsoup4 jinja2 ];
  };

  sphinx-book-theme = buildPythonPackage rec {
    pname = "sphinx-book-theme";
    version = "0.2.0";
    src = fetchPypi rec {
      inherit pname version;
      sha256 = "sha256-n5di8sY3Ln66rJBXB2KO/oA4bT5RYJRdiWXTCa7tlA0=";
    };
    # XXX Some upper bound bumps, doesn't seem to break anything
    patchPhase = ''
      sed -i \
        -e 's/docutils>=0.15,<0.17/docutils>=0.15,<0.18/' \
        -e 's/pydata-sphinx-theme~=0.7.2/pydata-sphinx-theme~=0.8.0/' \
        setup.py
    '';
    propagatedBuildInputs = with py.pkgs; [ pyyaml docutils beautifulsoup4 pydata-sphinx-theme ];
  };

  myst-parser = buildPythonPackage rec {
    pname = "myst-parser";
    version = "0.16.1";
    src = fetchPypi rec {
      inherit pname version;
      sha256 = "sha256-pkc7lzXIx0lZtJs2VQclRk9K7MRIE0DJpfkVOCkZH4M=";
    };
    propagatedBuildInputs = with py.pkgs; [ pyyaml markdown-it-py jinja2 docutils mdit-py-plugins sphinx ];
  };
}
