{ghc}:
with (import <nixpkgs> {});

let
  pinnednixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz) {
    config = config // { allowBroken = true; };
  };

in
  pinnednixpkgs.haskell.lib.buildStackProject {
    ghc = pinnednixpkgs.haskell.compiler.ghc967;
    name = "mailsh";
    buildInputs = with pinnednixpkgs; [
      git
      zlib
      system-sendmail
      icu
      pkg-config
    ];
  }
