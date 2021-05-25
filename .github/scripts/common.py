import globals
import logging
import subprocess
import requests
import time

# Exception class to raise when a basic workflow error happens
class WorkflowError(Exception):
    pass

def GetLogger():
    if (globals.logger == None):
        logging.basicConfig(level=logging.NOTSET)
        logging.root.setLevel(logging.DEBUG)
        globals.logger = logging.getLogger()
        globals.logger.setLevel(logging.DEBUG)
    return globals.logger

def RaiseWorkflowError(error):
    logging.shutdown()
    raise WorkflowError(error)

# This name needs to match what's in the build.yaml file
def GetGccBuildName():
    return 'build'

def GetandPrintCurrentSHA():
    res = subprocess.run('git rev-parse HEAD', shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    # Output for Github to pick up output for future jobs
    print("::set-output name=currentSHA::" + str(res.stdout, 'utf-8'))

def SendGetRestCmd(restCmd, restArgs, restHeader):
    logger = GetLogger()
    retryCount = 10
    while True:
        res = requests.get(restCmd, params=restArgs, headers=restHeader)
        if res.ok:
            return res

        # Retry the command after a sleep
        sleepTimeSeconds = 30
        logger.error("Sleeping for " + str(sleepTimeSeconds) + " seconds before retrying " + restCmd)
        time.sleep(sleepTimeSeconds)

        retryCount = retryCount - 1
        if retryCount == 0:
            RaiseWorkflowError("Max retry count hit with " + restCmd)