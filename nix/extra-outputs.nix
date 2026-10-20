{ cl-process-kit, paredit-cli }:
ctx:
let
  oraclePackages = import ./oracle-packages.nix { inherit (ctx) pkgs; };
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
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ oraclePackages;
    });
  };
}
