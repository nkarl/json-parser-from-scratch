{
  description = "A Nix flake for Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs, ... }:
    let

      system = "x86_64-linux";

    in
    {
      devShells."${system}".default =
        let
          pkgs = import nixpkgs {
            inherit system;
          };

        in
        pkgs.mkShell {
          packages = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            hlint
          ];
        };
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
    };
}
