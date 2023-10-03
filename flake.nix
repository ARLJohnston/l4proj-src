{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell
      {
          nativeBuildInputs = with pkgs; [
            haskell-language-server
            ghcid
            cabal-install
          ];
      };
      # Change the prompt to show that you are in a devShell
      shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
  };
}
