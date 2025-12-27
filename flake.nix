{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; };
          Scylla = import ./elm.nix {
            inherit (pkgs) lib stdenv sass;
            inherit (pkgs.elmPackages) fetchElmDeps elm;
            inherit (pkgs.nodePackages) uglify-js;
          };
        in
          {
            packages = { inherit Scylla; };
            defaultPackage = Scylla;
          }
        );
}

