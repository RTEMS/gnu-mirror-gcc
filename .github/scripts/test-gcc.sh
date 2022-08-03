# Copyright (c) Microsoft Corporation.

# MIT License

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

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


