{
  description = "Mathematical Components (the book)";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; {
        defaultPackage = stdenv.mkDerivation {
          name = "math-comp-book";
          version = "master";
          src = ./.;

          nativeBuildInputs = [ git texlive.combined.scheme-full ];

          buildPhase = ''
            cd tex
            make
            biber main
            make
            make
          '';

          installPhase = ''
            mkdir -p $out/share
            cp book.pdf $out/share
          '';

          meta = with lib; {
            description = "Mathematical components book";
            homepage = "https://github.com/math-comp/mcb";
            license = lib.licenses.cc-by-nc-40;
            maintainers = with maintainers; [ siraben ];
            platforms = platforms.all;
          };
        };
      }
    );
}

