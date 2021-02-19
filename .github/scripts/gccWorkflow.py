from config import *
import subprocess
from downloadBuildArtifact import *
import sys
import globals
from common import *
import os.path 

class GccWorkflow(object):
    
    # Setup the config object and wait for any runs necessary to finish before proceeding onto the next job section
    # to avoid exceeding the 6 hr limit on Github Actions that run on Github machines
    @staticmethod
    def Init(githubContext, accessToken, isInitForBuild=False):
        Config.Setup(githubContext, accessToken, isInitForBuild)

    # Runs the configure script to set up gcc configuration environment prior to building and running tests
    # Creates the objdir directory as part of this process
    @staticmethod
    def Configure():        
        logger = GetLogger()
        res = subprocess.run('''
            chmod +x .github/scripts/configure-gcc.sh
            .github/scripts/configure-gcc.sh''',
            shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

        logger.info("output = " + str(res.stdout, 'utf-8'))

    @staticmethod
    def Build(configJson):
        logger = GetLogger()
        logger.info("Gcc Build start")
        Config.Reload(configJson)

        GccWorkflow.Configure()

        # Build
        res = subprocess.run('''
            chmod +x .github/scripts/build-gcc.sh
            .github/scripts/build-gcc.sh''',
          shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
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
        
        if (not buildDownloaded):
            logger.info("Downloading gccBuild artifact from 'build' workflow...")
            try:
                DownloadBuildArtifact()
            except:
                logger.error("Could not download build artifact")
                RaiseWorkflowError("Error downloading build artifact for GCC workflow")
            
            GccWorkflow.Configure()
            res = subprocess.run('''
                mv gccBuild/* ../objdir -f
                ''',
                shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                
            logger.info("mv cmd output = " + str(res.stdout, 'utf-8'))

            if (res.returncode != 0):
                logger.error("Failed to copy gccBuild into objdir directory")    
                logging.shutdown()
                sys.exit(res.returncode)
        
        # The gcc build should be in the objdir folder one level above
        currentDir = os.getcwd()
        assert os.path.isdir(currentDir + "/../objdir")
        
        # Copy over downloaded build artifact into objdir directory that was created in the configure script
        # For more details on the subprocess function and its parameters, 
        # see https://stackoverflow.com/questions/4256107/running-bash-commands-in-python
        res = subprocess.run('''
            chmod +x .github/scripts/test-gcc.sh
            ''' 
            + ".github/scripts/test-gcc.sh " + testSet,
          shell=True, check=False, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        logger.info("test output = " + str(res.stdout, 'utf-8'))

        # TODO: Add better handling of errors to display
        if (res.returncode != 0):
            logger.error("GCC Test failed")
            logging.shutdown()
            sys.exit(res.returncode)
        
