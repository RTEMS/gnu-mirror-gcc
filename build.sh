#!/usr/bin/env bash

set -e
set -u

installdir=${1:-"gcc-inst"}
mkdir -p $HOME/code/gcc-build/
mkdir -p $HOME/code/${installdir}/
pushd $HOME/code/gcc-build/
if find . -mindepth 1 -print -quit 2>/dev/null | grep -q . ; then
	  echo "not empty?"
else
  $OLDPWD/configure --disable-bootstrap --disable-libsanitizer --enable-__cxa_atexit --enable-shared --disable-libsanitizer --enable-languages=c,c++,fortran --enable-lto --enable-gold --enable-linker-build-id --with-cpu-emag --prefix="$HOME/code/${installdir}/"
fi

make -j `nproc`
make install -j `nproc`
make check-gcc RUNTESTFLAGS="ipa.exp"
popd
