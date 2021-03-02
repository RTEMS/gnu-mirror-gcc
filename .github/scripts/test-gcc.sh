cd ../objdir

chmod +x $PWD -R

make -k $1 -j$(nproc) #RUNTESTFLAGS="-v -v"
../gcc/contrib/testsuite-management/validate_failures.py

exit_code=$?
if [ $exit_code != 0 ]; then
    exit $exit_code
fi

# To run one test in particular, use
#make check-gcc RUNTESTFLAGS="-v -v dg.exp=c-c++-common/asan/asan-interface-1.c"


