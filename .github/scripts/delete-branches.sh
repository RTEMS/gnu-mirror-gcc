# Deletes branches that match the regex and are older than the maximum acceptable branch age
# $1 is the regex to match the branch to
# $2 is the maximum acceptable branch age

branchesStr=$(git for-each-ref --format='%(refname:short) %(committerdate:relative)' --sort=-committerdate   refs/remotes/origin)
readarray -t branches <<<"$branchesStr"
maxAcceptableDateInSecs=$(date -d "$2 days ago" +%s)
regexMatch=$1

for (( i=0; i<${#branches[@]} ; i+=1 )) ; do
    echo "${branches[i]}"
    read -r branchName lastCommitRelative <<< "${branches[i]}"
    # Remove commas in the date which can occur with older branches. e.g. "2 years, 2 months ago"
    lastCommitRelative=$(echo "$lastCommitRelative" | sed 's;,;;' )
    lastCommitRelativeDate=$(date -d "$lastCommitRelative" +%s)

    echo "$lastCommitRelative"
    echo "$lastCommitRelativeDate"
    echo "$maxAcceptableDateInSecs"
    if [ $lastCommitRelativeDate -lt $maxAcceptableDateInSecs ]
    then
        branchNameTrimmed=$( echo "$branchName" | sed 's;origin/;;' )
        [[ $branchNameTrimmed =~ $regexMatch ]] && echo "Deleting branch $branchNameTrimmed" && git push origin --delete $branchNameTrimmed
    fi
done