cd $HOME/code
mkdir gcc-build
cd gcc-build
$HOME/code/ipa-dlo/gcc/configure --prefix=$HOME/code/gcc-inst --disable-bootstrap -enable-language=c,c++,lto --disable-multilib
make CFLAGS='-O0 -g' CXXFLAGS='-O0 -g' -j 12
make install
