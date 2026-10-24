{
  description = "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # cl-nix-forge supports asdf:program-op on Darwin, so aitools can use the
    # same delivery path on both platforms.
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Sibling package inputs use release tags; only their source trees are
    # needed by the lispDerivation outputs below.
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.4.0";
      flake = false;
    };

    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v2.2.0";
      flake = false;
    };

    cl-json-kit = {
      url = "github:nerima-lisp/cl-json-kit/v1.2.0";
      flake = false;
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      flake = false;
    };

    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.3.1";
      flake = false;
    };

    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.3.0";
      flake = false;
    };

    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      flake = false;
    };

    cl-process-kit = {
      url = "github:nerima-lisp/cl-process-kit/v3.4.0";
      flake = false;
    };

    cl-vcs-kit = {
      url = "github:nerima-lisp/cl-vcs-kit/v0.2.0";
      flake = false;
    };

    # These inputs satisfy transitive kit ASDF dependencies. Each kit is built
    # in its own derivation, so its dependencies belong in its own
    # lispDependencies list below.
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.2.0";
      flake = false;
    };

    cl-log-kit = {
      url = "github:nerima-lisp/cl-log-kit/v2.2.0";
      flake = false;
    };

    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      flake = false;
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.4.0";
      flake = false;
    };

    cl-crypto-kit = {
      url = "github:nerima-lisp/cl-crypto-kit/v0.1.1";
      flake = false;
    };

    cl-deflate-kit = {
      url = "github:nerima-lisp/cl-deflate-kit/v0.3.0";
      flake = false;
    };

    cl-resilience-kit = {
      url = "github:nerima-lisp/cl-resilience-kit/v1.0.0";
      flake = false;
    };

    cl-yaml-kit = {
      url = "github:nerima-lisp/cl-yaml-kit/v0.2.1";
      flake = false;
    };

    cl-toml-kit = {
      url = "github:nerima-lisp/cl-toml-kit/v0.2.0";
      flake = false;
    };

    cl-org-kit = {
      url = "github:nerima-lisp/cl-org-kit/v0.3.1";
      flake = false;
    };

    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/v1.1.0";
      flake = false;
    };

    # Consumed for its `lib` output (`mkLintCheck`), which a `flake = false`
    # source tree cannot provide.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.7.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      cl-nix-forge,
      cl-cli,
      cl-regex-kit,
      cl-json-kit,
      cl-codec-kit,
      cl-host-kit,
      cl-boundary-kit,
      cl-concurrent-kit,
      cl-process-kit,
      cl-vcs-kit,
      cl-date-kit,
      cl-log-kit,
      cl-parser-kit,
      cl-weave,
      cl-crypto-kit,
      cl-deflate-kit,
      cl-resilience-kit,
      cl-yaml-kit,
      cl-toml-kit,
      cl-org-kit,
      cl-observability-kit,
      paredit-cli,
      treefmt-nix,
    }:
    let
      # CI gates both systems listed here.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
    in
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit self systems nixpkgs;

      pname = "aitools";

      # Single source of truth for the version: the `:version` form in
      # aitools.asd.
      asd = ./aitools.asd;

      root = ./.;

      # Integration specs read SKILL.md, and nested ASDF systems need their
      # .asd files in the filtered source tree.
      sourceInclude = import ./nix/source-include.nix;
      # Exclude temporary scratch directories from path-based source builds.
      # Match their prefixes so clean checkouts need not contain them.
      sourceExclude =
        let
          scratchPrefixes = [
            ".release-probe."
            ".session-audit-"
            ".store-fd-probe."
          ];
          scratchDirs = nixpkgs.lib.filterAttrs (
            name: type:
            type == "directory" && nixpkgs.lib.any (prefix: nixpkgs.lib.hasPrefix prefix name) scratchPrefixes
          ) (builtins.readDir ./.);
        in
        map (name: ./. + "/${name}") (builtins.attrNames scratchDirs);

      meta = {
        description = "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends.";
        homepage = "https://github.com/nerima-lisp/aitools";
        license = nixpkgs.lib.licenses.mit;
        platforms = nixpkgs.lib.platforms.unix;
        mainProgram = "aitools";
      };

      lispDependencies = import ./nix/lisp-dependencies.nix inputs;

      # Test-only: cl-weave, the framework used by this project's specs.
      lispCheckDependencies = ctx: [
        (ctx.cl.lispDerivation {
          pname = "cl-weave";
          version = ctx.cl.fromAsdSystem "${cl-weave}/cl-weave.asd";
          src = cl-weave;
          lispSystem = "cl-weave";
        })
      ];

      # The delivered binary is owned by the separate AITOOLS/CLI ASDF
      # system. installSource lets it find its installed source tree if
      # ASDF needs to re-resolve the system at runtime.
      #
      # mkExecutable defaults to the ASDF system name "aitools/cli"; this
      # system's program-op writes to "aitools", so keep the output path aligned.
      executable = {
        installSource = true;
        lispSystem = "aitools/cli";
        programPath = "aitools";
      };

      # Give the Linux checks.default full-suite derivation 1800 seconds.
      timeoutSeconds = 1800;

      docs.root = ./docs;

      # The shared treefmt evaluation drives both `nix fmt` and
      # `checks.formatting`.
      treefmt.evalModule = treefmt-nix.lib.evalModule;

      overrideOutputs = import ./nix/delivery-overrides.nix {
        inherit cl-process-kit nixpkgs;
      };

      extraOutputs = import ./nix/extra-outputs.nix { inherit cl-process-kit paredit-cli; };
    };
}
