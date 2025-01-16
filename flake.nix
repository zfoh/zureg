{
  description = "zureg";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskell.packages.ghc96;
        package = haskell.callCabal2nix "zureg" ./. {
          urlencoded = (pkgs.haskell.lib.doJailbreak haskell.urlencoded);
        };
        packageExes = pkgs.haskell.lib.justStaticExecutables package;
        migrations = pkgs.stdenv.mkDerivation {
          name = "migrations";
          src = ./lib/Zureg/Database/Migrations;
          installPhase = ''
            mkdir -p $out/var/lib/zureg/migrations
            cp *.sql $out/var/lib/zureg/migrations
          '';
        };
      in {
        packages = {
          default = package;

          docker = pkgs.dockerTools.buildLayeredImage {
            name = "zureg";
            tag = "latest";
            contents = [
              pkgs.cacert
              migrations
            ];
            config.Cmd = "${packageExes}/bin/zureg-web";
          };
        };
        devShells = {
          default = let
            postgres = {
              db = "zureg";
              password = "hunter2";
              port = "5433";
            };
          in pkgs.mkShell {
            buildInputs = [ pkgs.zlib.dev ];
            packages = [
              pkgs.cabal-install
              pkgs.entr
              pkgs.jq
              pkgs.docker
              pkgs.postgresql
              pkgs.awscli2
              haskell.stylish-haskell
              (haskell.ghc.withPackages (p:
                package.buildInputs ++ [ p.postgresql-simple ]))
            ];

            shellHook = ''
              docker container port ${postgres.db}-postgres || docker run \
                --rm \
                --name ${postgres.db}-postgres \
                -e POSTGRES_DB=${postgres.db} \
                -e POSTGRES_PASSWORD=${postgres.password} \
                -p ${postgres.port}:5432 \
                -d postgres
            '';
          };
        };
        formatter = pkgs.nixfmt;
      });
}
