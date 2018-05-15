{ mkDerivation, base, pure-core, pure-txt, stdenv }:
mkDerivation {
  pname = "pure-lifted";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-txt ];
  homepage = "github.com/grumply/pure-lifted";
  license = stdenv.lib.licenses.bsd3;
}
