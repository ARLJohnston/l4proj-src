{
  description = "Basic DevShell for Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = pkgs.mkShell {
	    nativeBuildInputs = with pkgs; [
        ghc
        cabal-install
        stack
	    ];
	  };
  };

}
