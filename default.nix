let
  pkgs = import <nixpkgs> {};
  # Base virtualenv package from 20.03 does not seem to work correctly ...
  # See https://github.com/NixOS/nixpkgs/issues/66366
  # The definition below comes from the issue above
  virtualenv = pkgs.python3Packages.virtualenv.overridePythonAttrs (old: rec {
    pname = "virtualenv";
    version = "20.0.21";

    src = pkgs.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1kxnxxwa25ghlkpyrxa8pi49v87b7ps2gyla7d1h6kbz9sfn45m1";
    };

    propagatedBuildInputs = with pkgs.python3Packages; [
      appdirs distlib filelock setuptools_scm six contextlib2 importlib-metadata
      importlib-resources pathlib2
    ];

    patches = [];
  });
  my-python-packages = python-packages: [ virtualenv ];
  python-with-my-packages =
    pkgs.python37.withPackages my-python-packages;

in
pkgs.stdenv.mkDerivation {
  name = "talk";
  buildInputs = with pkgs; [
    texlive.combined.scheme-full pdfpc
    ocamlPackages.merlin
    ocamlPackages.ocaml
    python-with-my-packages
  ];
}
