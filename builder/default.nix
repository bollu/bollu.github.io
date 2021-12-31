{ pkgs ? import <nixpkgs> {}, katexPath, prismPath, inputPath, outputPath}:

with pkgs;


stdenv.mkDerivation {
	name = "builder";
	src = ./.;
	nativeBuildInputs = [ cmake ninja ];
# 	buildPhase = ''
# 		mkdir build; cd build; cmake -G Ninja ../.; ninja
# 	'';
# this combines configure and build into one phase, less than wholly cool

	cmakeFlags = "-G Ninja";
	postPatch = ''
		set -e
		sed -i --debug "s@const char CONFIG_KATEX_PATH.*@const char CONFIG_KATEX_PATH\[\]=\"${katexPath}\";@" builder.cpp
		sed  -i --debug "s@const char CONFIG_PRISM_PATH.*@const char CONFIG_PRISM_PATH\[\]=\"${prismPath}\";@" builder.cpp
		sed  -i --debug "s@const char CONFIG_OUTPUT_DIRECTORY_NO_TRAINING_SLASH.*@const char CONFIG_OUTPUT_DIRECTORY_NO_TRAINING_SLASH\[\]=\"${outputPath}\";@" builder.cpp
		sed  -i --debug "s@const char CONFIG_INPUT_MARKDOWN_PATH.*@const char CONFIG_INPUT_MARKDOWN_PATH\[\]=\"${inputPath}\";@" builder.cpp
	'';
	buildPhase = ''ninja'';



}
