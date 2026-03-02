#!/bin/bash
set -x
Z3_VERSION=$(cd "$(dirname "$0")" && dune exec ./z3_version.exe)
case $(uname -s) in
  Linux)
    JOB=build
    Z3_ARCH=x64-glibc-2.39
    Z3_DLL=libz3.so
    ;;
  Darwin)
    JOB=build-mac
    Z3_ARCH=arm64-osx-13.7.6
    Z3_DLL=libz3.dylib
    ;;

esac

# c-to-json:
wget -nv --content-disposition "https://gitlab.com/umb-svl/c-to-json/-/jobs/artifacts/main/raw/build/c-to-json-bin.tar.bz2?job=$JOB" -O c-to-json-bin.tar.bz2 &&
tar xvf c-to-json-bin.tar.bz2 &&
rm c-to-json-bin.tar.bz2 &&
# faial:
cp \
  ../faial-bc \
  ../c-ast \
  ../wgsl-ast \
  ../faial-drf \
  ../faial-cost-dyn \
  ../faial-cost \
  bin/ &&
cp ../scripts/faial-drf ../README.md ../LICENSE  ./ &&
# macOS-specific: copy GMP and update binary references
if [ "$OSTYPE" == "darwin"* ] || [ "$Z3_DLL" == "libz3.dylib" ]; then
  cp /opt/homebrew/opt/gmp/lib/libgmp.10.dylib bin/ &&
  for binary in bin/faial-drf bin/faial-bc bin/c-ast bin/wgsl-ast bin/faial-cost-dyn bin/faial-cost; do
    if [ -f "$binary" ]; then
      install_name_tool -change /opt/homebrew/opt/gmp/lib/libgmp.10.dylib @loader_path/libgmp.10.dylib "$binary"
    fi
  done
fi &&
# download z3
mkdir lib/ &&
wget -nv --content-disposition "https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VERSION}/z3-${Z3_VERSION}-${Z3_ARCH}.zip" &&
unzip z3-${Z3_VERSION}-${Z3_ARCH}.zip &&
cp z3-${Z3_VERSION}-${Z3_ARCH}/bin/${Z3_DLL} lib/ &&
cp z3-${Z3_VERSION}-${Z3_ARCH}/LICENSE.txt ./LICENSE-z3.txt &&
rm -rf z3-${Z3_VERSION}-${Z3_ARCH}.zip z3-${Z3_VERSION}-${Z3_ARCH}/ &&
# display tree & bundle:
tar jcvf faial.tar.bz2 * &&
exit 0
