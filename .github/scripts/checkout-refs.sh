# $1 is the reference to use for checking out GCC sources
# $2 is the reference to use for checking out the .github scripts
git checkout $1 -f
git fetch origin +refs/pull/*:refs/remotes/origin/refs/pull/*
git checkout $2 -- .github/