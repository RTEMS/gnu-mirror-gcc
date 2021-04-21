
cd ../objdir
$PWD/../gcc/configure --prefix=$HOME/GCC --enable-languages=c,c++,fortran

exit_code=$?
if [ $exit_code != 0 ]; then
    exit $exit_code
fi

make -j$(nproc)
exit_code=$?
if [ $exit_code != 0 ]; then
    exit $exit_code
fi

sudo make install
exit_code=$?
if [ $exit_code != 0 ]; then
    exit $exit_code
fi