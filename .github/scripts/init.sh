# Script to run basic initialization for developers and GitHub workflows

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

# Setup upstream gcc remote
git ls-remote --exit-code --quiet gcc > /dev/null 2>&1
if test $? != 0; then   
    echo "Adding gcc upstream remote"
    git remote add gcc git://gcc.gnu.org/git/gcc.git
else
    # Check if the URL ends with gcc.gnu.org. We don't check for a string explicitly
    # since some devs may use ssh or other means to connect to the remote
    gccUrl="$(git config remote.gcc.url)"

    if [[ "$gccUrl" != *"gcc.gnu.org/git/gcc.git" ]]; then
        echo "ERROR: gcc upstream remote was found but it's not pointing to git://gcc.gnu.org/git/gcc.git. Please delete the local gcc upstream remote before running init again"
        exit -1
    fi
fi