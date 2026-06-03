{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {self, nixpkgs}:
    let
      lp = nixpkgs.legacyPackages;
      sh = {mkShell, ghc, cabal-install, haskell-language-server, hlint, ...}:
        mkShell {
          packages = [ghc cabal-install haskell-language-server hlint];
        };
    in {
      
      packages.aarch64-darwin.default = lp.aarch64-darwin.callPackage sh {};
    };
}
