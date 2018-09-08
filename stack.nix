{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "mailsh";
  nativeBuildInputs = [ git ];
  buildInputs = [ zlib ncurses ssmtp ];
}
