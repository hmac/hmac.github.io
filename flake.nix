{
  description = "hmac.dev";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskellPackages;

        ghc = hPkgs.ghcWithPackages (ps: [ ps.hakyll ps.pandoc ]);

        build-binary = pkgs.runCommand "build" { nativeBuildInputs = [ ghc ]; } ''
            mkdir -p $out/bin tmp
            cp ${./build.hs} tmp/Main.hs
            ghc -O1 -outputdir tmp -o $out/bin/build tmp/Main.hs
          '';

        build-script = pkgs.writeShellApplication {
          name = "build";
          runtimeInputs = [ pkgs.graphviz ];
          text = ''exec ${build-binary}/bin/build "$@"'';
        };
    in
  {
    packages.default = build-script;
    apps.default = { type = "app"; program = "${build-script}/bin/build"; };

    devShells.default = pkgs.mkShell {
      buildInputs = [ ghc pkgs.graphviz ];
    };
  });
}
