{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "mailsh";
  buildInputs = [ zlib ncurses ssmtp ];
}
