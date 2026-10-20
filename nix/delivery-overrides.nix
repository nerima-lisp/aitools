{ cl-process-kit, nixpkgs }:
# `bg start` detaches through cl-process-kit's native spawn trampoline,
# which its lispDerivation (Lisp sources only) does not build. The
# delivered package carries a copy next to bin/aitools, where the
# process context looks for it relative to the running executable. The
# Linux check and Darwin apps.test runner set CL_PROCESS_KIT_SPAWN so
# the bg specs run instead of being skipped. Flags match cl-process-kit's
# own `spawnCflags`.
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
  oraclePackages = import ./oracle-packages.nix { inherit (ctx) pkgs; };
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
}
