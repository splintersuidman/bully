{ mkDerivation, base, bytestring, cassava, containers, directory
, filepath, lens, lib, mtl, pandoc, pandoc-types, text, time
, tomland, unordered-containers, wreq
}:
mkDerivation {
  pname = "bulletin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava containers directory filepath lens mtl
    pandoc pandoc-types text time tomland unordered-containers wreq
  ];
  executableHaskellDepends = [
    base bytestring cassava containers directory filepath lens mtl
    pandoc pandoc-types text time tomland unordered-containers wreq
  ];
  license = lib.licenses.gpl3Plus;
}
