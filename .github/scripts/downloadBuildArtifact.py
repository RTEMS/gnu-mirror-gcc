# Requirements:
# sudo apt-get install python-pip
# sudo apt-get install python
# python -m pip install requests

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

import requests
import json
from datetime import datetime
import datetime as dt
import zipfile
import time
from common import *
import globals
import re
import tarfile
import os
import subprocess

def WaitOnLatestWorkflow(branch, workflowName, accessToken, raiseErrorOnFailure):
    logger = GetLogger()
    logger.info("Looking for a " + workflowName + " build for branch: " + branch)

    latestWorkflowTime = dt.datetime(1970, 1, 1)
    latestWorkflowId = 0

    while latestWorkflowId == 0:
        # Look at all workflow runs on the branch
        res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/runs?branch=" + branch, {}, {'Authorization': "token " + accessToken})
        runs = res.json()['workflow_runs']

        # This could be optimized if we can guarantee that the results are sorted. It seems like the results are sorted from most recent to least recent, but there
        # doesn't seem to be any documentation supporting this.

        for entry in runs:
            if (entry['name'] == workflowName):
                date = datetime.strptime(entry['created_at'], '%Y-%m-%dT%H:%M:%SZ')
                if (latestWorkflowTime < date):
                    latestWorkflowTime = date
                    latestWorkflowId = entry['id']

    
    logger.info("Found workflow ID: " + str(latestWorkflowId))
    buildCompleted = False
    conclusion = ''
    # Wait until latestWorkflowId completes
    while buildCompleted == False:
        res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/runs/" + str(latestWorkflowId), {}, {'Authorization': "token " + accessToken})
        if res.ok:
            resJson = res.json()
            status = resJson['status']
            if (status == 'completed'):
                buildCompleted = True
                conclusion = resJson['conclusion']

        if not buildCompleted:
            # Sleep for 30 seconds
            sleepTimeSeconds = 30
            logger.error("Sleeping for " + str(sleepTimeSeconds) + " seconds while waiting on build number " + str(latestWorkflowId) + " to finish")
            time.sleep(sleepTimeSeconds)

    logger.info("Run completed. Link available at:")
    logger.info("https://github.com/microsoft/gcc/actions/runs/" + str(latestWorkflowId))

    if raiseErrorOnFailure:
        if conclusion != 'success':
            RaiseWorkflowError("Workflow did not succeed. Conclusion was: " + conclusion)

def FindGccBuildArtifact(workflowRunID, accessToken):
    logger = GetLogger()
    numArtifactRetries = 60
    while True:
        res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/runs/" + str(workflowRunID) + "/artifacts", {}, {'Authorization': "token " + accessToken})
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

def WaitOnGccBuild(commit, accessToken):
    logger = GetLogger()
    logger.info("Looking for a gcc build for " + commit)
    
    res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/commits/" + commit + "/check-runs", {'check_name': GetGccBuildName()}, {'Authorization': "token " + accessToken})
    checkruns = res.json()['check_runs']
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

    while True:
        # Get the run ID
        res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/jobs/" + str(latestBuildId), {}, {'Authorization': "token " + accessToken})
        jobJson = res.json()
        workflowRunID = jobJson['run_id']

        if jobJson['status'] == 'completed':
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

    gccBuildArtifactID = FindGccBuildArtifact(workflowRunID, accessToken)

    return gccBuildArtifactID

# Returns a list of artifact objects in the runId that match the regex
def GetArtifactObjsInRun(runId, regex):
    r = re.compile(regex)
    matchedList = []
    
    res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/runs/" + str(runId) + "/artifacts", {}, {'Authorization': "token " + globals.configObj.accessToken})
    artifactsList = res.json()['artifacts']
    for artifact in artifactsList:
        name = artifact['name']
        if r.match(name):
            matchedList.append(artifact)
    
    return matchedList

def DownloadArtifact(artifactID):
    logger = GetLogger()
    res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/artifacts/" + str(artifactID), {}, {'Authorization': "token " + globals.configObj.accessToken})
    name = res.json()['name']
    zipName = name + '.zip'

    res = SendGetRestCmd("https://api.github.com/repos/microsoft/gcc/actions/artifacts/" + str(artifactID) +"/zip", {}, {'Authorization': "token " + globals.configObj.accessToken})
    
    logger.info("Downloading " + name + " zip from artifact ID " + str(artifactID))
    with open(zipName, 'wb') as f:
        f.write(res.content)

    logger.info("Unzipping zip")
    with zipfile.ZipFile(zipName, 'r') as zip_ref:
        zip_ref.extractall(name)
    print('currentCWD')
    print(os.getcwd())
    logger.info("Done downloading")

    return name

def DownloadBuildArtifact():
    logger = GetLogger()

    if (globals.configObj.gccBuildArtifactID):
        gccBuildArtifactID = globals.configObj.gccBuildArtifactID
    else:
        gccBuildArtifactID = FindGccBuildArtifact(globals.configObj.runID, globals.configObj.accessToken)

    # TODO: Support downloading build artifact without a config object setup so the script can be used outside of workflows by developers
    filename = DownloadArtifact(gccBuildArtifactID)
    res = subprocess.run('mv ' + filename + '/* ../objdir -f ',
        shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    logger.info("mv cmd output = " + str(res.stdout, 'utf-8'))

