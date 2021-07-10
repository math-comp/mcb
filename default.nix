with import <nixpkgs> {};

stdenv.mkDerivation rec {
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
}
