{ lib,
  haskellPackages,
  git,
  zlib,
  system-sendmail,
  icu,
  pkg-config
}:

let haskellPackages' = haskellPackages.override {
  overrides = self: super: {
    purebred-email = self.callPackage ./purebred-email.nix {};
  };
};
in haskellPackages'.mkDerivation rec {
  pname = "mailsh";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = with haskellPackages'; [
    attoparsec attoparsec-expr base base64-string bytestring
    case-insensitive containers directory esqueleto filepath hostname
    html-conduit html-entities iconv lens mime-mail mime-types mmorph
    monad-logger mtl old-locale old-time parsec persistent
    persistent-sqlite persistent-template process purebred-email random
    safecopy text text-icu time time-compat url uuid wl-pprint
    xml-conduit
  ];
  libraryToolDepends = with haskellPackages'; [ hpack ];
  executableHaskellDepends = with haskellPackages; [
    ansi-terminal ansi-wl-pprint attoparsec attoparsec-expr base
    base64-string bytestring case-insensitive containers directory
    esqueleto filepath gitrev hostname html-conduit html-entities iconv
    lens mime-mail mime-types mmorph monad-logger mtl old-locale
    old-time optparse-applicative parsec persistent persistent-sqlite
    persistent-template process purebred-email random safecopy tar
    terminal-size text text-icu time time-compat url uuid vector
    wcwidth wl-pprint word-wrap xml-conduit zippers
  ];
  testHaskellDepends = with haskellPackages'; [
    attoparsec attoparsec-expr base base64-string bytestring
    case-insensitive containers directory esqueleto filepath
    generic-random hostname html-conduit html-entities iconv lens
    mime-mail mime-types mmorph monad-logger mtl old-locale old-time
    parsec persistent persistent-sqlite persistent-template process
    purebred-email QuickCheck quickcheck-instances random safecopy
    tasty tasty-hunit tasty-quickcheck text text-icu time time-compat
    url uuid wl-pprint xml-conduit
  ];
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/talanis85/mailsh#readme";
  description = "Mail at the shell";
  license = lib.licenses.bsd3;
  mainProgram = "mailsh";

  executableToolDepends = [
    system-sendmail
  ];

  executableSystemDepends = [
    git
    zlib
    icu
    pkg-config
  ];

  librarySystemDepends = executableSystemDepends;
}
