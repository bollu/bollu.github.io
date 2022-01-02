{ pkgs ? import <nixpkgs> {}}:

with pkgs;

stdenv.mkDerivation {
	buildInputs = let
		builder = pkgs.callPackage ./builder { 
			katexPath = ./katex/katex.min.js; 
			prismPath = ./prism/prism.js;
			inputPath = ./README.md;
			outputPath = /var/bollu.github.io/.;
		};
	in [ builder ];
	name = "bollu.github.io";
	src = ./.;
	buildPhase = "builder";
}
