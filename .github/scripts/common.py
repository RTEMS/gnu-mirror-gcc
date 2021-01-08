import globals
import logging

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