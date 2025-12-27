{ lib, stdenv, elm, fetchElmDeps, uglify-js, sass }:

let
  mkDerivation =
    { srcs ? ./elm-dependencies.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./registry.dat
    , outputJavaScript ? false
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elm sass ]
        ++ lib.optional outputJavaScript uglify-js;

      buildPhase = fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        extension = if outputJavaScript then "js" else "html";
      in ''
        ${lib.concatStrings (map (module: ''
          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --optimize --output $out/${module}.${extension}
          ${lib.optionalString outputJavaScript ''
            echo "minifying ${elmfile module}"
            uglifyjs $out/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                | uglifyjs --mangle --output $out/${module}.min.${extension}
          ''}
        '') targets)}

	# Custom logic for Scylla in particular
	mkdir $out/static $out/static/js $out/static/css $out/static/svg
	cp $src/index.html $out/index.html
	cp $out/Main.min.js $out/static/js/elm.js
	cp $src/static/js/*.js $out/static/js
	cp $src/static/svg/*.svg $out/static/svg
	sass $src/static/scss/style.scss $out/static/css/style.css
      '';
    };
in mkDerivation {
  name = "Scylla-0.1.0";
  srcs = ./elm-dependencies.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "./src";
  outputJavaScript = true;
}

