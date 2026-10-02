{
  description = "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # v0.6.0: mkExecutable drives asdf:program-op on every platform,
    # including aarch64-darwin, and $out/bin/<pname> is the real executable
    # program-op wrote (no wrapper unless native libraries need one). This
    # delivers a single Darwin binary for aitools's purposes -- verified
    # directly (`git log v0.5.0..v0.6.0`: "feat(nix): deliver a real
    # executable on Darwin through program-op"), not assumed from a message
    # alone.
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Every sibling package below is pinned to a release tag, never a bare
    # `github:nerima-lisp/<pkg>` (which would follow that repo's default
    # branch and break this repo's CI without warning). `flake = false`: only
    # the source tree is needed, to build a `lispDerivation` below, never
    # these repos' own flake outputs.
    cl-cli = {
      url = "github:nerima-lisp/cl-cli/v1.4.0";
      flake = false;
    };

    # v2.0.0 already contains the bounded advanced executor (backreferences,
    # lookaround) and the Rust-style replace templates ($1, ${name}, $$)
    # aitools' regex search relies on -- verified directly against that
    # tag (src/advanced-*.lisp, src/api-replace.lisp, and README.md's own
    # description are all present there, not only on a later commit).
    # v2.1.0 is a strict superset: `git diff --name-status v2.0.0 v2.1.0`
    # adds src/literal-prefilter.lisp and src/lazy-dfa.lisp (a required-literal
    # prefilter and a lazy DFA for match detection) with no change to
    # cl-regex-kit.asd's own :depends-on, which is exactly the
    # requirement that cl-regex-kit itself gain literal prefiltering and a
    # lazy DFA -- useful to the
    # future `search` context, not required by anything built in this task.
    # v2.1.1 (pinned here) adds only bug fixes over v2.1.0 (`git log
    # v2.1.0..v2.1.1`: "keep lower-priority Pike VM matches and bound byte
    # word-boundary cost", touching src/pike-vm-capture.lisp and
    # src/text-boundaries.lisp), which unblock the two search regex specs left
    # it-todo under v2.1.0.
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

    # Transitive-only: no aitools system names these in :depends-on, but
    # cl-concurrent-kit.asd, cl-process-kit.asd, and cl-vcs-kit.asd (via
    # cl-log-kit.asd) each name ONE of these in their OWN :depends-on, and
    # Nix builds every lispDerivation as its own sandboxed derivation, so
    # each kit's dependencies must be satisfied by ITS OWN lispDependencies
    # list below -- flattening these into aitools's own list would not reach
    # a nested build.
    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.1.1";
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

    # Consumed for its `lib` output (`mkLintCheck`), which a `flake = false`
    # source tree cannot provide.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.6.3";
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
      paredit-cli,
      treefmt-nix,
    }:
    let
      # x86_64-linux is what CI gates; aarch64-darwin is the development
      # machine. CI only requires x86_64-linux; aarch64-darwin is included
      # because cl-nix-forge v0.6.0's `mkExecutable` now drives a real
      # `asdf:program-op` on Darwin too (see the cl-nix-forge input comment
      # above), so building or running aitools on this platform is not blocked.
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

      # mkLispSource does not retain nested ASDF files automatically. Keep the
      # context definitions beside the aggregate system so its bootstrap can
      # register them in the build source as well as in a checkout.
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

      meta = {
        description = "An AI-agent-oriented replacement for cat/grep/sed/find/jq/tar and friends.";
        homepage = "https://github.com/nerima-lisp/aitools";
        license = nixpkgs.lib.licenses.mit;
        platforms = nixpkgs.lib.platforms.unix;
        mainProgram = "aitools";
      };

      # Runtime dependency closure for both the "aitools" library system and
      # the separate "aitools/cli" executable. These are BUILT DERIVATIONS,
      # not CL_SOURCE_REGISTRY strings -- cl-nix-forge assembles the registry
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
          (ctx.cl.lispDerivation {
            pname = "cl-cli";
            version = ctx.cl.fromAsdSystem "${cl-cli}/cl-cli.asd";
            src = cl-cli;
            lispSystem = "cl-cli";
            lispDependencies = [ hostKit ];
          })
        ];

      # Test-only: cl-weave, the org's test framework everywhere.
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
      # programPath must be given explicitly: mkExecutable's own default is
      # the ASDF SYSTEM NAME ("aitools/cli", which is not even a valid
      # pathname fragment), not :build-pathname. aitools.asd's "aitools/cli"
      # system has `:pathname "."` (repo root) and `:build-pathname
      # "aitools"`, so ASDF's program-op writes the binary to
      # "<root>/aitools" -- verified locally via `asdf:operate
      # 'asdf:program-op "aitools/cli"'`, which produced exactly that file.
      executable = {
        installSource = true;
        lispSystem = "aitools/cli";
        programPath = "aitools";
      };

      # The full integration and e2e suite exceeds cl-nix-forge's 600-second
      # default on aarch64-darwin. Keep the test gate bounded, but allow the
      # supported Darwin build to finish without weakening any assertions.
      timeoutSeconds = 1800;

      docs.root = ./docs;

      # ONE treefmt evaluation drives both `nix fmt` and `checks.formatting`.
      treefmt.evalModule = treefmt-nix.lib.evalModule;

      # `bg start` detaches through cl-process-kit's native spawn trampoline,
      # which its lispDerivation (Lisp sources only) does not build. The
      # delivered package carries a copy next to bin/aitools, where the
      # process context looks for it relative to the running executable, and
      # the test check points CL_PROCESS_KIT_SPAWN at it so the bg specs run
      # instead of skipping. Flags match cl-process-kit's own `spawnCflags`.
      overrideOutputs =
        ctx:
        let
          spawnTrampoline = ctx.pkgs.runCommandCC "cl-process-kit-spawn" { } ''
            mkdir -p $out/bin
            $CC -std=c11 -O2 -Wall -Wextra -Werror ${cl-process-kit}/native/spawn.c \
              -o $out/bin/cl-process-kit-spawn
          '';

          # Dump the delivered image in TWO SBCL processes instead of one.
          # cl-nix-forge's mkExecutable drives `asdf:program-op` in a single
          # process, which compiles every fasl and dumps the core in that same
          # process; under SBCL 2.6.0's mark-region GC the compile-time
          # fragmentation stays in the dumped core (~159 MB measured locally).
          # Compiling the fasls in one process and dumping from a FRESH process
          # that only LOADS them yields ~109 MB and ~23% fewer --version
          # startup instructions, with identical output. cl-nix-forge cannot be
          # asked for this split, so it is done here from ctx.lispDerivationArgs
          # (the exact attrset the preset fed lispDerivation, which mkExecutable
          # documents as what its own `args` wants) -- aitools.asd is unchanged.
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
          # Stage 2: run program-op over that pre-compiled tree in a fresh
          # process. The fasls carry the same normalized store mtime as their
          # sources, so ASDF finds them current and loads them without
          # recompiling (verified locally: the dumping process runs zero
          # compile-file calls), and the dumped core never shares a heap with
          # the compiler.
          twoStageExecutable = ctx.cl.mkExecutable {
            inherit (ctx.cl) lispDerivation;
            args = ctx.lispDerivationArgs // {
              lispSystem = "aitools/cli";
              src = cliFasls;
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
          testCheck = ctx.generated.checks.default.overrideAttrs (old: {
            CL_PROCESS_KIT_SPAWN = "${spawnTrampoline}/bin/cl-process-kit-spawn";
            AITOOLS_E2E_BINARY = "${delivered}/bin/aitools";
            AITOOLS_DARWIN_PS = "${ctx.pkgs.darwin.ps}/bin/ps";
            TZDIR = "${ctx.pkgs.tzdata}/share/zoneinfo";
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
              ctx.pkgs.git
              ctx.pkgs.zip
              ctx.pkgs.unzip
              ctx.pkgs.jq
              ctx.pkgs.perl
              ctx.pkgs.which
              ctx.pkgs.procps
              ctx.pkgs.darwin.ps
              ctx.pkgs.lsof
              ctx.pkgs.util-linux
              ctx.pkgs.openssl
              ctx.pkgs.inetutils
              ctx.pkgs.getconf
              ctx.pkgs.tzdata
            ];
          });
        in
        {
          packages.default = delivered;
          apps.default = deliveredApp;
          apps.aitools = deliveredApp;
          # AITOOLS_E2E_BINARY makes t/e2e exercise the delivered package
          # instead of building its own copy in the sandbox.
          # git, zip and unzip are the oracles the gitignore-parity, vcs,
          # archive-interop and e2e tests compare against; the rest back the
          # correspondence-table e2e oracles (jq for json/table; perl for the
          # invisible/normalize/blame rows; which/ps/lsof/getconf/uuidgen/
          # hostname/openssl for the sys and util rows). Without them each of
          # those specs reports a counted skip instead of comparing aitools
          # against the real tool. TZDIR points the time-zone specs at tzdata's
          # zoneinfo so the --tz rows resolve Asia/Tokyo in the sandbox.
          checks.default = testCheck;
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
            # Structural parse gate over every Lisp source in the filtered
            # tree: fails if any .lisp/.asd file is not a balanced S-expression
            # document, catching an unbalanced components.sexp-driven file
            # before ASDF fails to load the system with a confusing error.
            paredit-lint = paredit-cli.lib.${ctx.system}.mkLintCheck {
              inherit (ctx) src;
              name = "aitools-paredit-lint";
            };
            coverage = ctx.generated.checks.default.overrideAttrs (old: {
              AITOOLS_COVERAGE = "1";
              CL_PROCESS_KIT_SPAWN = "${spawnTrampoline}/bin/cl-process-kit-spawn";
              AITOOLS_E2E_BINARY = "${ctx.generated.packages.default}/bin/aitools";
              AITOOLS_DARWIN_PS = "${ctx.pkgs.darwin.ps}/bin/ps";
              TZDIR = "${ctx.pkgs.tzdata}/share/zoneinfo";
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                ctx.pkgs.git
                ctx.pkgs.zip
                ctx.pkgs.unzip
                ctx.pkgs.jq
                ctx.pkgs.perl
                ctx.pkgs.which
                ctx.pkgs.procps
                ctx.pkgs.darwin.ps
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
