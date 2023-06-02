with import <nixpkgs> {};

let
  myGhc = ghc.withPackages(p : with p; [vector JuicyPixels fsnotify xml]);
in stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [ myGhc libusb pkgconfig zlib ];
  shellHook = ''
    export NIX_GHC=${myGhc}/bin/ghc
    export NIX_GHC_LIBDIR=${myGhc}/lib/ghc-${myGhc.version}
  '';
}
