final: previous:
with final.haskell.lib;

{
  pactPackages =
    let
      pactPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
      pactPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (pactPkg name);
      pactPkgWithOwnComp = name: pactPkgWithComp name name;

    in
    {
      "pact-web-server" = pactPkg "pact-web-server";
    };

  pactRelease =
    final.symlinkJoin {
      name = "pact-release";
      paths = final.lib.attrValues final.pactPackages;
    };


  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );
                  appendfulRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "appendful";
                      rev = "98d1a191941f94fa0379d5c08371ba0963d3462e";
                      sha256 = "sha256:1lkxhpn1cvxjqa4v45k9b0n9hgw1psvs40abp09gqrc3009v974l";
                    };
                  appendfulPkg = name: self.callCabal2nix "appendful" (appendfulRepo + "/${name}") { };
                  base16Repo =
                    final.fetchFromGitHub {
                      owner = "emilypi";
                      repo = "base16";
                      rev = "f340b4a9a496320010930368e503ba6b7907f725";
                      sha256 = "sha256:1c6910h9y3nmj2277d7bif3nilgacp4qafl4g5b3r2c0295hbq7z";
                    };
                  base16Pkg = self.callCabal2nix "base16" base16Repo { };

                in
                final.pactPackages // {
                  envparse = envparsePkg;
                  appendful = appendfulPkg "appendful";
                  appendful-persistent = appendfulPkg "appendful-persistent";
                  genvalidity-appendful = appendfulPkg "genvalidity-appendful";
                  base16 = base16Pkg;
                }
            );
      }
    );
}
