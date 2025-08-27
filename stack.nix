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
      cairo
      gtk3
      pango
      pkg-config
      xorg.xorgproto
      xorg.libX11

      libdatrie
      libepoxy
      libthai
      libselinux
      libsepol
      libxkbcommon
      libsysprof-capture
      pcre
      pcre2
      glib
      xorg.libXdmcp
      xorg.libxcb
      xorg.libXtst
      util-linux
      lerc
    ];
  }
