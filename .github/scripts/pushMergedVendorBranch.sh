# This script pushes the merged branch 
# $1 is the temporary local branch to create 
# $2 is the merge branch produced by the update-main.yaml workflow

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

BASEDIR=$(dirname $0)

chmod +x ${BASEDIR}/init.sh
${BASEDIR}/init.sh
if test $? != 0; then
  echo "ERROR: Initialization failed"
  exit -1
fi

if [[ `git status --porcelain` ]]; then
  echo "ERROR: You have local changes in your repository. Please stash or push them to a branch before running this script"
  exit -1
fi

echo "
---- READ THIS ---- 
When prompted for the 'local name for upstream repository', 
enter 'gcc'. The other fields can be the default
---- END OF READ THIS ----
"

${BASEDIR}/../../contrib/gcc-git-customization.sh
${BASEDIR}/../../contrib/git-fetch-vendor.sh --enable-push microsoft
git fetch
git fetch gcc
git fetch origin
git checkout -B microsoft/main vendors/microsoft/main
git merge origin/$1
git push vendors/microsoft microsoft/main

