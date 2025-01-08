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
        haskell = pkgs.haskell.packages.ghc94;
      in {
        packages = { default = haskell.callCabal2nix "zureg" ./. { }; };
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
                inputs.self.packages.${system}.default.buildInputs
                ++ [ p.postgresql-simple ]))
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

            ZUREG_DB =
              "postgresql://postgres:${postgres.password}@localhost:${postgres.port}/${postgres.db}";
          };
        };
        formatter = pkgs.nixfmt;
      });
}
