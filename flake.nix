{
  description = "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # cl-nix-forge v0.6.0 added Darwin support for building the executable
    # with asdf:program-op, so aitools can use the same delivery path there.
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
      url = "github:nerima-lisp/cl-org-kit/v0.1.0";
      flake = false;
    };

    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/v1.0.0";
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
    {
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
      sourceInclude = [
        ./skills/aitools/SKILL.md
        ./packages/core/kernel/kernel.asd
        ./packages/core/protocol/protocol.asd
        ./packages/core/store/store.asd
        ./packages/core/text/text.asd
        ./packages/core/workspace/workspace.asd
        ./packages/feature/edit/edit.asd
        ./packages/feature/env/env.asd
        ./packages/feature/inspect/inspect.asd
        ./packages/feature/journal/journal.asd
        ./packages/feature/process/process.asd
        ./packages/feature/search/search.asd
        ./packages/feature/util/util.asd
        ./packages/feature/vcs/vcs.asd
      ];
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

      # Runtime dependency closure for both the "aitools" library system and
      # the separate "aitools/cli" executable. These are built derivations,
      # not CL_SOURCE_REGISTRY strings; cl-nix-forge assembles the registry
      # transitively from them.
      lispDependencies =
        ctx:
        let
          dateKit = ctx.cl.lispDerivation {
            pname = "cl-date-kit";
            version = ctx.cl.fromAsdSystem "${cl-date-kit}/cl-date-kit.asd";
            src = cl-date-kit;
            lispSystem = "cl-date-kit";
          };
          hostKit = ctx.cl.lispDerivation {
            pname = "cl-host-kit";
            version = ctx.cl.fromAsdSystem "${cl-host-kit}/cl-host-kit.asd";
            src = cl-host-kit;
            lispSystem = "cl-host-kit";
          };
          codecKit = ctx.cl.lispDerivation {
            pname = "cl-codec-kit";
            version = ctx.cl.fromAsdSystem "${cl-codec-kit}/cl-codec-kit.asd";
            src = cl-codec-kit;
            lispSystem = "cl-codec-kit";
          };
          parserKit = ctx.cl.lispDerivation {
            pname = "cl-parser-kit";
            version = ctx.cl.fromAsdSystem "${cl-parser-kit}/cl-parser-kit.asd";
            src = cl-parser-kit;
            lispSystem = "cl-parser-kit";
          };
          jsonKit = ctx.cl.lispDerivation {
            pname = "cl-json-kit";
            version = ctx.cl.fromAsdSystem "${cl-json-kit}/cl-json-kit.asd";
            src = cl-json-kit;
            lispSystem = "cl-json-kit";
          };
          boundaryKit = ctx.cl.lispDerivation {
            pname = "cl-boundary-kit";
            version = ctx.cl.fromAsdSystem "${cl-boundary-kit}/cl-boundary-kit.asd";
            src = cl-boundary-kit;
            lispSystem = "cl-boundary-kit";
            lispDependencies = [ hostKit ];
          };
          concurrentKit = ctx.cl.lispDerivation {
            pname = "cl-concurrent-kit";
            version = ctx.cl.fromAsdSystem "${cl-concurrent-kit}/cl-concurrent-kit.asd";
            src = cl-concurrent-kit;
            lispSystem = "cl-concurrent-kit";
            lispDependencies = [
              boundaryKit
              dateKit
            ];
          };
          logKit = ctx.cl.lispDerivation {
            pname = "cl-log-kit";
            version = ctx.cl.fromAsdSystem "${cl-log-kit}/cl-log-kit.asd";
            src = cl-log-kit;
            lispSystem = "cl-log-kit";
            lispDependencies = [
              dateKit
              concurrentKit
              hostKit
            ];
          };
          regexKit = ctx.cl.lispDerivation {
            pname = "cl-regex-kit";
            version = ctx.cl.fromAsdSystem "${cl-regex-kit}/cl-regex-kit.asd";
            src = cl-regex-kit;
            lispSystem = "cl-regex-kit";
            lispDependencies = [
              concurrentKit
              parserKit
            ];
          };
          processKit = ctx.cl.lispDerivation {
            pname = "cl-process-kit";
            version = ctx.cl.fromAsdSystem "${cl-process-kit}/cl-process-kit.asd";
            src = cl-process-kit;
            lispSystem = "cl-process-kit";
            lispDependencies = [
              boundaryKit
              logKit
              codecKit
              concurrentKit
            ];
          };
          vcsKit = ctx.cl.lispDerivation {
            pname = "cl-vcs-kit";
            version = ctx.cl.fromAsdSystem "${cl-vcs-kit}/cl-vcs-kit.asd";
            src = cl-vcs-kit;
            lispSystem = "cl-vcs-kit";
            lispDependencies = [
              processKit
              hostKit
              logKit
            ];
          };
          cryptoKit = ctx.cl.lispDerivation {
            pname = "cl-crypto-kit";
            version = ctx.cl.fromAsdSystem "${cl-crypto-kit}/cl-crypto-kit.asd";
            src = cl-crypto-kit;
            lispSystem = "cl-crypto-kit";
          };
          deflateKit = ctx.cl.lispDerivation {
            pname = "cl-deflate-kit";
            version = ctx.cl.fromAsdSystem "${cl-deflate-kit}/cl-deflate-kit.asd";
            src = cl-deflate-kit;
            lispSystem = "cl-deflate-kit";
          };
          resilienceKit = ctx.cl.lispDerivation {
            pname = "cl-resilience-kit";
            version = ctx.cl.fromAsdSystem "${cl-resilience-kit}/cl-resilience-kit.asd";
            src = cl-resilience-kit;
            lispSystem = "cl-resilience-kit";
            lispDependencies = [
              boundaryKit
              concurrentKit
              dateKit
            ];
          };
          yamlKit = ctx.cl.lispDerivation {
            pname = "cl-yaml-kit";
            version = ctx.cl.fromAsdSystem "${cl-yaml-kit}/cl-yaml-kit.asd";
            src = cl-yaml-kit;
            lispSystem = "cl-yaml-kit";
            lispDependencies = [
              regexKit
              codecKit
            ];
          };
          tomlKit = ctx.cl.lispDerivation {
            pname = "cl-toml-kit";
            version = ctx.cl.fromAsdSystem "${cl-toml-kit}/cl-toml-kit.asd";
            src = cl-toml-kit;
            lispSystem = "cl-toml-kit";
            lispDependencies = [
              parserKit
              dateKit
              codecKit
            ];
          };
          orgKit = ctx.cl.lispDerivation {
            pname = "cl-org-kit";
            version = ctx.cl.fromAsdSystem "${cl-org-kit}/cl-org-kit.asd";
            src = cl-org-kit;
            lispSystem = "cl-org-kit";
            lispDependencies = [
              parserKit
              regexKit
              codecKit
              dateKit
            ];
          };
          observabilityKit = ctx.cl.lispDerivation {
            pname = "cl-observability-kit";
            version = ctx.cl.fromAsdSystem "${cl-observability-kit}/cl-observability-kit.asd";
            src = cl-observability-kit;
            lispSystem = "cl-observability-kit";
            lispDependencies = [
              boundaryKit
              concurrentKit
            ];
          };
        in
        [
          jsonKit
          regexKit
          codecKit
          hostKit
          boundaryKit
          concurrentKit
          processKit
          vcsKit
          cryptoKit
          deflateKit
          resilienceKit
          yamlKit
          tomlKit
          orgKit
          observabilityKit
          (ctx.cl.lispDerivation {
            pname = "cl-cli";
            version = ctx.cl.fromAsdSystem "${cl-cli}/cl-cli.asd";
            src = cl-cli;
            lispSystem = "cl-cli";
            lispDependencies = [ hostKit ];
          })
        ];

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

      # `bg start` detaches through cl-process-kit's native spawn trampoline,
      # which its lispDerivation (Lisp sources only) does not build. The
      # delivered package carries a copy next to bin/aitools, where the
      # process context looks for it relative to the running executable. The
      # Linux check and Darwin apps.test runner set CL_PROCESS_KIT_SPAWN so
      # the bg specs run instead of being skipped. Flags match cl-process-kit's
      # own `spawnCflags`.
      overrideOutputs =
        ctx:
        let
          spawnTrampoline = ctx.pkgs.runCommandCC "cl-process-kit-spawn" { } ''
            mkdir -p $out/bin
            $CC -std=c11 -O2 -Wall -Wextra -Werror ${cl-process-kit}/native/spawn.c \
              -o $out/bin/cl-process-kit-spawn
          '';

          # Compile the fasls and dump the executable in separate SBCL
          # processes so compile-time heap state does not carry into the core.
          #
          # Stage 1: compile every fasl in the aitools/cli closure. The
          # lispDerivation default build-op is `asdf:load-system`, so this
          # compiles and loads but never dumps; its $out is the built tree with
          # fasls beside their sources.
          cliFasls = ctx.cl.lispDerivation (
            ctx.lispDerivationArgs
            // {
              pname = "aitools-cli-fasls";
              lispSystem = "aitools/cli";
            }
          );
          # Stage 2: run program-op in a fresh process over the pre-compiled tree.
          twoStageExecutable = ctx.cl.mkExecutable {
            inherit (ctx.cl) lispDerivation;
            args = ctx.lispDerivationArgs // {
              lispSystem = "aitools/cli";
              src = cliFasls;
              # Stage 1 already patched these sources before compilation.
              postPatch = "";
            };
            programPath = "aitools";
            installSource = true;
          };
          delivered = ctx.pkgs.runCommand "aitools-${ctx.version}" { inherit (twoStageExecutable) meta; } ''
            cp -R ${twoStageExecutable}/. $out
            chmod -R u+w $out
            cp ${spawnTrampoline}/bin/cl-process-kit-spawn $out/bin/
          '';
          deliveredApp = {
            type = "app";
            program = "${delivered}/bin/aitools";
            inherit (ctx.generated.apps.default) meta;
          };
          oraclePackages =
            with ctx.pkgs;
            [
              git
              gnugrep
              zip
              unzip
              jq
              perl
              which
              procps
              lsof
              util-linux
              openssl
              inetutils
              getconf
              tzdata
              xxd
              ripgrep
              fd
              tree
              dos2unix
              nkf
              netcat
              bc
              python3
            ]
            ++ nixpkgs.lib.optionals ctx.pkgs.stdenv.hostPlatform.isLinux [ ctx.pkgs.nettools ]
            ++ nixpkgs.lib.optionals ctx.pkgs.stdenv.hostPlatform.isDarwin [ ctx.pkgs.unixtools.netstat ];
          testDependencies = ctx.package.enableCheck.ancestry.deps;
          testRegistry = nixpkgs.lib.concatStringsSep ":" (
            [ "${ctx.src}//" ]
            ++ map (dependency: "${dependency}${dependency.registrySuffix or ""}") testDependencies
          );
          hostTestRunner = ctx.pkgs.writeShellApplication {
            name = "aitools-host-tests";
            text = ''
              # Prefer native Darwin ps/top for host process visibility; grep -P
              # uses GNU grep.
              export PATH=${
                if ctx.pkgs.stdenv.hostPlatform.isDarwin then
                  "${nixpkgs.lib.makeBinPath [ ctx.pkgs.gnugrep ]}:/usr/bin:/bin:/usr/sbin:/sbin:"
                else
                  ""
              }${nixpkgs.lib.makeBinPath (oraclePackages ++ [ ctx.pkgs.coreutils ])}
              export CL_SOURCE_REGISTRY=${nixpkgs.lib.escapeShellArg testRegistry}
              export CL_PROCESS_KIT_SPAWN=${spawnTrampoline}/bin/cl-process-kit-spawn
              export AITOOLS_E2E_BINARY=${delivered}/bin/aitools
              export TZDIR=${ctx.pkgs.tzdata}/share/zoneinfo
              AITOOLS_HOST_CACHE_DIR=$(${ctx.pkgs.coreutils}/bin/mktemp -d -t aitools-host-tests.XXXXXX)
              export AITOOLS_HOST_CACHE_DIR
              exec ${ctx.pkgs.coreutils}/bin/timeout -k 30s 1800s \
                ${ctx.pkgs.sbcl}/bin/sbcl --noinform --non-interactive --no-sysinit --no-userinit \
                --load ${ctx.src}/run-host-tests.lisp --end-toplevel-options
            '';
          };
          testCheck = ctx.generated.checks.default.overrideAttrs (old: {
            CL_PROCESS_KIT_SPAWN = "${spawnTrampoline}/bin/cl-process-kit-spawn";
            AITOOLS_E2E_BINARY = "${delivered}/bin/aitools";
            AITOOLS_DARWIN_PS = if ctx.system == "aarch64-darwin" then "${ctx.pkgs.darwin.ps}/bin/ps" else "";
            TZDIR = "${ctx.pkgs.tzdata}/share/zoneinfo";
            nativeBuildInputs =
              (old.nativeBuildInputs or [ ])
              ++ oraclePackages
              ++ ctx.pkgs.lib.optionals (ctx.system == "aarch64-darwin") [
                ctx.pkgs.darwin.ps
              ];
          });
        in
        {
          packages.default = delivered;
          apps.default = deliveredApp;
          apps.aitools = deliveredApp;
          apps.test = {
            type = "app";
            program = "${hostTestRunner}/bin/aitools-host-tests";
            meta.description = "Run every spec against the Nix delivery on the host; no skips or todos";
          };
          # Run end-to-end specs against the delivered package with the
          # external tools and time-zone data they require.
          # Linux's checks.default runs the full suite. Darwin's checks.default
          # validates delivery and schema; Darwin CI runs all specs via apps.test.
          checks.default =
            if ctx.pkgs.stdenv.hostPlatform.isDarwin then
              ctx.pkgs.runCommand "aitools-delivery-check" { nativeBuildInputs = [ ctx.pkgs.jq ]; } ''
                test -x ${hostTestRunner}/bin/aitools-host-tests
                test -x ${delivered}/bin/cl-process-kit-spawn
                ${delivered}/bin/aitools schema > schema.json
                jq -e '.status == "ok" and (.commands | length > 0) and
                  ([.commands[].name] | length == (unique | length))' schema.json
                mkdir -p "$out"
                cp schema.json "$out/"
              ''
            else
              testCheck;
        };

      extraOutputs =
        ctx:
        let
          spawnTrampoline = ctx.pkgs.runCommandCC "cl-process-kit-spawn" { } ''
            mkdir -p $out/bin
            $CC -std=c11 -O2 -Wall -Wextra -Werror ${cl-process-kit}/native/spawn.c \
              -o $out/bin/cl-process-kit-spawn
          '';
        in
        {
          checks = {
            # Check balanced delimiters in every .lisp and .asd file in the
            # filtered tree before ASDF loads the systems.
            paredit-lint = paredit-cli.lib.${ctx.system}.mkLintCheck {
              inherit (ctx) src;
              name = "aitools-paredit-lint";
            };
          }
          // ctx.pkgs.lib.optionalAttrs (!ctx.pkgs.stdenv.hostPlatform.isDarwin) {
            coverage = ctx.generated.checks.default.overrideAttrs (old: {
              AITOOLS_COVERAGE = "1";
              CL_PROCESS_KIT_SPAWN = "${spawnTrampoline}/bin/cl-process-kit-spawn";
              AITOOLS_E2E_BINARY = "${ctx.generated.packages.default}/bin/aitools";
              AITOOLS_DARWIN_PS = if ctx.system == "aarch64-darwin" then "${ctx.pkgs.darwin.ps}/bin/ps" else "";
              TZDIR = "${ctx.pkgs.tzdata}/share/zoneinfo";
              nativeBuildInputs =
                (old.nativeBuildInputs or [ ])
                ++ [
                  ctx.pkgs.git
                  ctx.pkgs.zip
                  ctx.pkgs.unzip
                  ctx.pkgs.jq
                  ctx.pkgs.perl
                  ctx.pkgs.which
                  ctx.pkgs.procps
                ]
                ++ ctx.pkgs.lib.optionals (ctx.system == "aarch64-darwin") [
                  ctx.pkgs.darwin.ps
                ]
                ++ [
                  ctx.pkgs.lsof
                  ctx.pkgs.util-linux
                  ctx.pkgs.openssl
                  ctx.pkgs.inetutils
                  ctx.pkgs.getconf
                  ctx.pkgs.tzdata
                ];
            });
          };
        };
    };
}
