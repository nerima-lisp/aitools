;;;; data/domain/search/build-files-data.lisp
;;;;
;;;; The file names `overview` reports as build files.
;;;; Each entry is a wildmatch pattern tested against a file's base name.
(in-package #:aitools.data)

(defparameter *search-build-file-patterns*
  '("flake.nix" "default.nix" "shell.nix"
    "*.asd"
    "Cargo.toml"
    "go.mod"
    "package.json" "deno.json" "deno.jsonc"
    "pyproject.toml" "setup.py" "setup.cfg" "requirements.txt"
    "Gemfile" "*.gemspec"
    "pom.xml" "build.gradle" "build.gradle.kts" "settings.gradle" "settings.gradle.kts"
    "Makefile" "GNUmakefile" "CMakeLists.txt" "meson.build" "configure.ac"
    "build.zig" "dune-project" "mix.exs" "rebar.config" "stack.yaml" "*.cabal"
    "composer.json" "*.csproj" "*.sln"
    "Dockerfile" "Justfile" "justfile" "Taskfile.yml")
  "Base-name patterns of build and project definition files.")

(export '*search-build-file-patterns*)
