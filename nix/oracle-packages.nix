{ pkgs }:
with pkgs;
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
++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.nettools ]
++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.unixtools.netstat ]
