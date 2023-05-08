{ghc}:
with (import <nixpkgs> {});

let
  pinnednixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz) {
    config = config // { allowBroken = true; };
  };

in
  pinnednixpkgs.haskell.lib.buildStackProject {
    ghc = pinnednixpkgs.haskell.compiler.ghc884;
    name = "mailsh";
    buildInputs = with pinnednixpkgs; [
      git
      zlib
      ssmtp
      icu
      cairo
      gnome3.gtk
      pango
      pkgconfig
      xorg.xorgproto
      xorg.libX11
    ];
  }
