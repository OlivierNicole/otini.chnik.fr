{
  description = "Hakyll with Nix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  nixConfig.extra-substituters = [
    "https://cache.iog.io"
  ];

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' rec {
              src = ./.;
              compiler-nix-name = "ghc984";
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
      in
        flake // {
          packages.default = flake.packages."otini-chnik-fr:exe:site";
          apps.default = {
            type = "app";
            program = "${flake.packages."otini-chnik-fr:exe:site"}/bin/site";
          };
        }
    );
}
