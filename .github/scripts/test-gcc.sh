cd ../objdir

chmod +x $PWD -R
mkdir logs

# Run tests
make -k $1 -j$(nproc) RUNTESTFLAGS="-v -v" 2>&1 >> logs/output.txt 2>&1

# Compare failures found to the failures that we're expecting
../gcc/contrib/testsuite-management/validate_failures.py >> logs/failures.txt 2>&1
exit_code=$?

# Print failures to console output so they can be viewed in the GitHub Action run
cat logs/failures.txt

if [ $exit_code != 0 ]; then
    exit $exit_code
fi

# To run one test in particular, use
#make check-gcc RUNTESTFLAGS="-v -v dg.exp=c-c++-common/asan/asan-interface-1.c"


