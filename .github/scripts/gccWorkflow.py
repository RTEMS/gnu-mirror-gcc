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

from config import *
import subprocess
from downloadBuildArtifact import *
import sys
import globals
from common import *
import os
import json
import glob

class GccWorkflow(object):
    # Setup the config object and wait for any runs necessary to finish before proceeding onto the next job section
    # to avoid exceeding the 6 hr limit on Github Actions that run on Github machines
    @staticmethod
    def Init(githubContext, accessToken, isInitForBuild=False):
        Config.Setup(githubContext, accessToken, isInitForBuild)

    @staticmethod
    def PrintTestSet():
        dictionary = {
            "testSet" : ["check-target-libstdc++-v3", "check-gcc-c++", "check-gcc-c", "check-target-libgomp", "check-target-libitm", "check-target-libatomic"]
        }
        dictionaryJson = json.dumps(dictionary)
        print("::set-output name=testSet::" + dictionaryJson)

    # Runs the configure script to set up gcc configuration environment prior to building and running tests
    # Creates the objdir directory as part of this process
    @staticmethod
    def Configure():        
        logger = GetLogger()
        res = RunScript(".github/scripts/configure-gcc.sh", False)
        logger.info("output = " + str(res.stdout, 'utf-8'))

    @staticmethod
    def MakeObjDir():
        logger = GetLogger()
        res = subprocess.run('''
            cd ..
            mkdir objdir
            ''',
            shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

        logger.info("output = " + str(res.stdout, 'utf-8'))

    @staticmethod
    def Build(configJson):
        logger = GetLogger()
        logger.info("Gcc Build start")
        Config.Reload(configJson)

        GccWorkflow.Configure()
        GccWorkflow.MakeObjDir()

        # Build
        res = RunScript(".github/scripts/build-gcc.sh", False)
        logger.info("build output = " + str(res.stdout, 'utf-8'))

        # TODO: Add better handling of errors to display
        if (res.returncode != 0):
            logger.error("GCC Build failed")
            logging.shutdown()
            sys.exit(res.returncode)

    @staticmethod
    def Test(configJson, testSet, accessToken, buildDownloadedStr):
        logger = GetLogger()
        logger.info("Gcc Test start")

        # Convert the input string (because that's how it's passed via the composite template) into a boolean
        buildDownloaded = buildDownloadedStr == 'True' or buildDownloadedStr == 'true'
        Config.Reload(configJson, accessToken)
        
        GccWorkflow.Configure()
        
        if (not buildDownloaded):
            logger.info("Downloading gccBuild artifact from 'build' workflow...")
            GccWorkflow.MakeObjDir()
            try:
                DownloadBuildArtifact()
            except:
                logger.error("Could not download build artifact")
                RaiseWorkflowError("Error downloading build artifact for GCC workflow")
        
        # The gcc build should be in the objdir folder one level above
        currentDir = os.getcwd()
        assert os.path.isdir(currentDir + "/../objdir")
        
        # Copy over downloaded build artifact into objdir directory that was created in the configure script
        # For more details on the subprocess function and its parameters, 
        # see https://stackoverflow.com/questions/4256107/running-bash-commands-in-python
        res = RunScript(".github/scripts/test-gcc.sh", False, testSet)
        logger.info("test output = " + str(res.stdout, 'utf-8'))

        # TODO: Add better handling of errors to display
        if (res.returncode != 0):
            globals.configObj.failedWorkflowError = "GCC Test failed"
            globals.configObj.failedReturnCode = res.returncode

        Config.PrintNoSecretConfigJson()
           
    @staticmethod
    def ParseTestLogs(configJson, accessToken):
        logger = GetLogger()
        logger.info("GCC Parsing Test Logs")

        Config.Reload(configJson, accessToken)

        # Load in the tests known to fail in our vendor branch
        with open('contrib/testsuite-management/x86_64-pc-linux-gnu.xfail') as xFailFile:
            linesFailFile = {line.rstrip('\n') for line in xFailFile}

        newFailuresToAdd = []

        # Look for logs in the artifacts directory that were downloaded in the yaml via download-artifact
        logDirs = glob.glob('artifacts/master_*_logs')

        for logDir in logDirs:
            logger.info ("Parsing log directory: " + logDir)

            # Each directory should have a failures.txt file
            assert os.path.exists(logDir + '/failures.txt')

            # Create a rawFailures.txt file with just the unexpected failure lines which are between the
            # "Unexpected results in this build" and "Expected results not present in this build" markers in the file
            res = subprocess.run('cd ' + logDir                
                + '''
                sed '0,/^Unexpected results in this build/d' failures.txt > afterUnexpected.txt
                sed '/^Expected results not present in this build/q' afterUnexpected.txt > rawFailures.txt
                sed -r '/^\s*$/d' -i rawFailures.txt
                ''',
                shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            logger.info("sed output = " + str(res.stdout, 'utf-8'))

            with open(logDir + '/rawFailures.txt') as rawFailuresFile:
                failsInThisDir = {line.rstrip('\n') for line in rawFailuresFile}
            newFails = failsInThisDir - linesFailFile

            for newFail in newFails:
                newFailuresToAdd.append(newFail)

        if len(newFailuresToAdd) == 0:
            logger.info("No new failures detected")
            return

        # Sort the list of failures so that they're in a predictable order
        newFailuresToAdd.sort()

        with open('contrib/testsuite-management/x86_64-pc-linux-gnu.xfail', 'a') as xFailFile:
            logger.info("New failures found:")
            for newFail in newFailuresToAdd:
                logger.info(newFail)
                xFailFile.write("\n")
                xFailFile.write(newFail)

        # Create a patch file
        res = subprocess.run('''
                git diff contrib/testsuite-management/x86_64-pc-linux-gnu.xfail > newFailures.patch
                ''',
                shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        logger.info("git diff output = " + str(res.stdout, 'utf-8'))
        
        res = subprocess.run('''
                git add contrib/testsuite-management/x86_64-pc-linux-gnu.xfail
                git commit -m "Update xfail with new failures"
                ''',
                shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        logger.info("git add and commit output = " + str(res.stdout, 'utf-8'))
