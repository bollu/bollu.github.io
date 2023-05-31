# { pkgs ? import <nixpkgs> {}, katexPath, prismPath, inputPath, outputPath}:
{ pkgs ? import <nixpkgs> {} }:

with pkgs;


stdenv.mkDerivation {
	name = "builder";
	src = ./.;
	nativeBuildInputs = [ cmake ninja python3 ccmake ];
	cmakeFlags = "-DBLOG_ROOT_FOLDER_TRAILING_SLASH=XX";
	buildPhase = "make";

}
