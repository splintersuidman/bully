{ mkDerivation, aeson, base, base64, bytestring, containers
, exceptions, lib, mtl, pandoc, pandoc-types, servant-server, text
, time, vector, wai, warp, witch
}:
mkDerivation {
  pname = "bully";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64 bytestring containers exceptions mtl pandoc
    pandoc-types servant-server text time vector wai warp witch
  ];
  executableHaskellDepends = [ base ];
  license = lib.meta.getLicenseFromSpdxId "GPL-3.0-or-later";
  mainProgram = "bully-server";
}
