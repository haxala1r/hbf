{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {self, nixpkgs}:
    let
      lp = nixpkgs.legacyPackages;
      sh = {mkShell, ghc, cabal-install, haskell-language-server, hlint, ...}:
        mkShell {
          packages = [ghc cabal-install haskell-language-server hlint];
        };
      pkg = {haskellPackages, ...}:
        haskellPackages.developPackage {
          root = ./.;
        };
    in {
      packages.x86_64-linux.default = lp.x86_64-linux.callPackage pkg {};
      packages.aarch64-linux.default = lp.aarch64-linux.callPackage pkg {};
      packages.aarch64-darwin.default = lp.aarch64-darwin.callPackage pkg {};

      devShells.x86_64-linux.default = lp.x86_64-linux.callPackage sh {};
      devShells.aarch64-linux.default = lp.aarch64-linux.callPackage sh {};
      devShells.aarch64-darwin.default = lp.aarch64-darwin.callPackage sh {};
    };
}
