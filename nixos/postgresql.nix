{ config, lib, pkgs, ... }:

let cfg = config.valdaro.postgresql;
    users = cfg.users;

    options = {
      enable = lib.mkEnableOption "Postgresql service with user & db provisioning";

      users = lib.mkOption {
        type = lib.types.attrsOf userType;
        default = {};
      };
    };

    userType = lib.types.submodule {
      options = {
        databases = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = "databases for this user. The databases will be created.";
          default = [];
        };

        superuser = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };

        initialPassword = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = "Password hashed with scram-sha-256.";
          default = null;
        };
      };
    };

    service = {
      enable = true;
      inherit authentication ensureDatabases ensureUsers;
      settings.listen_addresses = "localhost";
    };

    authentication = lib.strings.concatMapStrings authenticate (builtins.attrNames users);

    authenticate = userName:
      if builtins.isString users.${userName}.initialPassword
      then
        ''
          local all ${userName}                                                scram-sha-256
        '' +
        lib.strings.optionalString cfg.available-on-tissue ''
          host  all ${userName} ${config.personal-infrastructure.tissue.ip}/24 scram-sha-256
        ''
      else
        ''
          local all ${userName} peer
        '';

    ensureDatabases = lib.lists.unique (lib.lists.concatMap (u: u.databases) (builtins.attrValues users));

    ensureUsers = lib.attrsets.mapAttrsToList buildUser users;

    buildUser = name: def: {
      inherit name;
      ensureClauses = ensureClauses def;
    };

    ensureClauses = def: {
      inherit (def) superuser;
      createrole = def.superuser;
      createdb   = def.superuser;
    };

    initialScript = ''
      $PSQL -tAf ${setPasswordsSQLFile}
      $PSQL -tAf ${grantPermissionSQLFile}
    '';

    setPasswordsSQLFile =
      pkgs.writeText "set-passwords.sql"
      (lib.strings.concatStrings (lib.attrsets.mapAttrsToList mkAlterRoleStatement users));

    mkAlterRoleStatement = name: def:
      lib.strings.optionalString (builtins.isString def.initialPassword) ''
        DO $do$
        BEGIN
          IF EXISTS (SELECT * FROM pg_shadow WHERE usename = '${name}' AND passwd IS null)
          THEN
            RAISE NOTICE 'Setting password for user ${name}';
            ALTER ROLE "${name}" WITH PASSWORD '${def.initialPassword}';
          ELSE
            RAISE NOTICE 'Nothing to do for user ${name}';
          END IF;
        END $do$;
      '';

    grantPermissionSQLFile =
      pkgs.writeText "grant-permissions.sql"
        (lib.strings.concatStrings (lib.attrsets.mapAttrsToList grantUserDatabases users));

    grantUserDatabases = name: def:
      lib.strings.concatMapStrings (grantUserDatabase name) def.databases;

    grantUserDatabase = name: db:
      "GRANT ALL PRIVILEGES ON DATABASE ${db} TO ${name};";
in
{
  options.valdaro.postgresql = options;

  config = lib.mkIf cfg.enable {
    services.postgresql = service;
    systemd.services.postgresql.postStart = initialScript;
  };
}
