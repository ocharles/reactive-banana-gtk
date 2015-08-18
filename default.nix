{ mkDerivation, base, mtl, reactive-banana, stdenv, transformers, gtk3, 
reflection, tagged, tagged-transformer
}:
mkDerivation {
  pname = "material-ui";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base mtl reactive-banana transformers gtk3 reflection tagged
    tagged-transformer
  ];
  license = stdenv.lib.licenses.unfree;
}
