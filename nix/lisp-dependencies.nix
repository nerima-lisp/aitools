# Runtime dependency closure for both the "aitools" library system and
# the separate "aitools/cli" executable. These are built derivations,
# not CL_SOURCE_REGISTRY strings; cl-nix-forge assembles the registry
# transitively from them.
inputs:
ctx:
let
  inherit (inputs)
    cl-date-kit
    cl-host-kit
    cl-codec-kit
    cl-parser-kit
    cl-json-kit
    cl-boundary-kit
    cl-concurrent-kit
    cl-log-kit
    cl-regex-kit
    cl-process-kit
    cl-vcs-kit
    cl-crypto-kit
    cl-deflate-kit
    cl-resilience-kit
    cl-yaml-kit
    cl-toml-kit
    cl-org-kit
    cl-observability-kit
    cl-cli;
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
]

