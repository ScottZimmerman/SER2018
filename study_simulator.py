#----------------------------------------------------------------------
# Notes
#----------------------------------------------------------------------
# Main file of the study simulator system.

#----------------------------------------------------------------------
# Imports
#----------------------------------------------------------------------
import sys
import os
import json
import subprocess
import pprint

#----------------------------------------------------------------------
# Input
#----------------------------------------------------------------------
print("\n===================================================================")
print("==========================STUDY SIMULATOR==========================")
print("===================================================================\n")

if(len(sys.argv) != 3):
	print("Usage: python %s <data_dir> <job_dir>" % sys.argv[0])
	exit()

data_dir = sys.argv[1]
job_dir = sys.argv[2]

def runFeatureDist(jobID, data_dir):
	callList = ["Rscript","Feature_Distribution.R", data_dir, job_dir]

	p = subprocess.Popen(callList)
	p.wait()
	res = p.communicate()
	return(p.returncode)

def runFeatureImportance(jobID, data_dir):
	callList = ["Rscript","Feature_Importance.R", data_dir, job_dir]

	p = subprocess.Popen(callList)
	print("REMOVE THIS FOR PARALLEL")
	p.wait()
	res = p.communicate()


def runFeatureSearch(jobID, data_dir):
	callList = ["Rscript","Feature_Search.R", data_dir, job_dir]

	p = subprocess.Popen(callList)
	
	print("REMOVE THIS FOR PARALLEL")
	p.wait()
	res = p.communicate()

import combine_instructions_components
combine_instructions_components.combine_instructions_components(data_dir,job_dir)

#----------------------------------------------------------------------
# Run job
#----------------------------------------------------------------------
with open(os.path.join("data",data_dir,"jobs",job_dir,"job.json")) as job_file:
    job = json.load(job_file)

jobID = data_dir
if job["type"] == "featureDist":
	returncode = runFeatureDist(jobID, data_dir)

elif job["type"] == "featureImportance":
	runFeatureImportance(jobID, data_dir)

elif job["type"] == "featureSearch":
	runFeatureSearch(jobID, data_dir)

else:
	print("Job type not recognized: "+str(job["type"]))
