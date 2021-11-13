# This is the "Mathematical Components" book.
[![build](https://github.com/math-comp/mcb/actions/workflows/build.yml/badge.svg)](https://github.com/math-comp/mcb/actions/workflows/build.yml)

## Building
To build the book using Nix, run
```ShellSession
# without flakes, check out the repo first
$ NIXPKGS_ALLOW_UNFREE=1 nix-build
$ with flakes
$ NIXPKGS_ALLOW_UNFREE=1 nix build github:math-comp/mcb --impure
```

Alternatively you may fetch the latest artifact produced by the CI for
the master branch
[here](https://github.com/math-comp/mcb/actions?query=branch%3Amaster).

The `tex/` directory contains the sources.  TexLive 2014, 2021 is
known to work.

The `coq/` directory contains snippets corresponding to the chapters
of the book.

The `docs/` directory contains the website of the book.

The `artwork/` directory contains the graphics used in the book.

### Homepage

Link to the [homepage of the book](https://math-comp.github.io/mcb)

