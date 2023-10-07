{ config, lib, ... }:

let serviceOptions = lib.types.submodule {
      options = {
        frontend = lib.mkOption {
          type = lib.types.path;
        };

        entrypoint = lib.mkOption {
          type = lib.types.path;
        };

        backendPort = lib.mkOption {
          type = lib.types.port;
          default = 12001;
        };

        sqlFixtures = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
        };

        environment = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = {};
        };

        serviceDependencies = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          example = [ "postgresql.service" ];
        };
      };
    };

    services = config.valdaro.services;

    hasServices = builtins.length (builtins.attrValues services) > 0;

    forServices = f: lib.lists.foldr lib.attrsets.recursiveUpdate {} (lib.attrsets.mapAttrsToList f services);

    nginxForService = name: svc:
      {
        enable = true;
        virtualHosts."${name}" = {
          root = "${svc.frontend}/share/assets";
          locations."/api/".proxyPass = "http://localhost:${toString svc.backendPort}/";
        };
      };

    systemdForService = name: svc:
      {
        "${name}-server" = {
          after = [ "network.target" ] ++ svc.serviceDependencies;
          wantedBy = [ "multi-user.target" ];
          environment = {
            PORT = toString svc.backendPort;
          } // svc.environment;
          preStart =
            if svc.sqlFixtures != null
            then "${svc.entrypoint} migrate --fixtures ${svc.sqlFixtures}"
            else "${svc.entrypoint} migrate";
          script = "${svc.entrypoint} serve";
        };
      };
 in

{
  options = {
    valdaro.services = lib.mkOption {
      type = lib.types.attrsOf serviceOptions;
      default = {};
    };
  };

  config = lib.mkIf hasServices {
    services.nginx   = forServices nginxForService;
    systemd.services = forServices systemdForService;
  };
}
