# Requirements:
# sudo apt-get install python-pip
# sudo apt-get install python
# python -m pip install requests

import requests
import json
from datetime import datetime
import datetime as dt
import zipfile
import time
from common import *
import globals

def WaitOnGccBuild(commit, accessToken):
    logger = GetLogger()
    logger.info("Looking for a gcc build for " + commit)
    
    reqArgs = {'check_name': GetGccBuildName()}
    res = requests.get("https://api.github.com/repos/microsoft/gcc/commits/" + commit + "/check-runs", params=reqArgs, headers={'Authorization': "token " + accessToken})

    checkruns = res.json()['check_runs']
    count = 0
    latestBuildTime = dt.datetime(1970, 1, 1)
    latestBuildId = 0

    for entry in checkruns:
        if (entry['name'] == GetGccBuildName()):
            date = datetime.strptime(entry['started_at'], '%Y-%m-%dT%H:%M:%SZ')
            
            # take the most recent build run
            if (latestBuildTime < date):
                latestBuildTime = date
                latestBuildId = entry['id']
    
    if (latestBuildId == 0):
        RaiseWorkflowError("No run found")

    logger.info("Found latest build workflow ID: " + str(latestBuildId))

    buildStatus = ''
    while True:
        # Get the run ID
        res = requests.get("https://api.github.com/repos/microsoft/gcc/actions/jobs/" + str(latestBuildId), headers={'Authorization': "token " + accessToken})
        jobJson = res.json()
        workflowRunID = jobJson['run_id']
        buildStatus = jobJson['status']
        if (buildStatus == 'completed'):
            logger.debug(jobJson)
            conclusion = jobJson['conclusion']
            if (conclusion == 'success'):
                break
            else:
                logger.error("Job conclusion is: " + conclusion)
                RaiseWorkflowError("Build run ID " + str(latestBuildId) + " failed")
        else:
            # Sleep for 30 seconds
            sleepTimeSeconds = 30
            logger.error("Sleeping for " + str(sleepTimeSeconds) + " seconds while waiting on build number " + str(latestBuildId) + " to finish")
            time.sleep(sleepTimeSeconds)

    logger.info("Workflow run ID = " + str(workflowRunID))

    numArtifactRetries = 60
    # Add error checking for the number of artifacts
    while True:
        res = requests.get("https://api.github.com/repos/microsoft/gcc/actions/runs/" + str(workflowRunID) + "/artifacts", headers={'Authorization': "token " + accessToken})

        logger.debug("Artifact Json:" + json.dumps(res.json()))

        gccBuildArtifactID = 0
        for artifact in res.json()['artifacts']:
            if (artifact['name'] == "gccBuild"):
                gccBuildArtifactID = artifact['id']

        if (gccBuildArtifactID != 0):
            break
        elif (gccBuildArtifactID == 0 and numArtifactRetries == 0):
            RaiseWorkflowError("No gcc build artifact found")

        numArtifactRetries = numArtifactRetries-1

        # We need this sleep here because even if the build job status is "completed", the gccBuild artifacts aren't
        # immediately ready. If for some reason they aren't, sleep for a bit and retry.
        
        # Sleep for 30 seconds
        sleepTimeSeconds = 30
        logger.info("Sleeping for " + str(sleepTimeSeconds) + " seconds while waiting on artifacts for workflow run ID " + str(workflowRunID))
        time.sleep(sleepTimeSeconds)

    logger.info("GCC Build artifact ID = " + str(gccBuildArtifactID))

    return gccBuildArtifactID

def DownloadBuildArtifact():
    logger = GetLogger()
    gccBuildArtifactID = globals.configObj.gccBuildArtifactID
    # TODO: Support downloading build artifact without a config object setup so the script can be used outside of workflows by developers
    res = requests.get("https://api.github.com/repos/microsoft/gcc/actions/artifacts/" + str(gccBuildArtifactID) +"/zip", headers={'Authorization': "token " + globals.configObj.accessToken})
    
    logger.info("Downloading gccBuild zip from artifact ID" + str(gccBuildArtifactID))
    with open('gccBuild.zip', 'wb') as f:
        f.write(res.content)

    logger.info("Unzipping zip")
    with zipfile.ZipFile('gccBuild.zip', 'r') as zip_ref:
        zip_ref.extractall('gccBuild')

    logger.info("Done downloading")
