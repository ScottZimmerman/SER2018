#----------------------------------------------------------------------
# Notes
#----------------------------------------------------------------------
# Create the directory structure for a new scenario

#----------------------------------------------------------------------
# Imports
#----------------------------------------------------------------------
import sys
import os
import json

#----------------------------------------------------------------------
# Input
#----------------------------------------------------------------------
if(len(sys.argv) != 3):
	print("Usage: python %s <scenario_name> <job_name>" % sys.argv[0])
	exit()
python_version = sys.version_info[0]

scenario_name = sys.argv[1]
job_name = sys.argv[2]
if job_name == "NONE":
	print("Job name cannot be \"CANCEL\"")
	exit()

scenario_dir = os.path.join("data",scenario_name)
job_dir = os.path.join(scenario_dir,"jobs",job_name)
print(scenario_dir)
print(os.path.exists(scenario_dir))
print(scenario_dir)
if not os.path.exists(scenario_dir):
	print("Error creating new job:")
	print("A scenario named \""+scenario_name+"\" does not exist")
	exit()

if not os.path.exists(job_dir):
	os.mkdir(job_dir)
	print("Created scenario '" + scenario_name + "'")
else:
	print("Error creating new job:")
	print("A job named \""+job_name+"\" already exists for the scenario \""+scenario_name+"\"")
	exit()

finished = False
while not finished:
	print("\nWhat type of job would you like to create?")
	print("\"fd\": feature distribution")
	print("\"fi\": feature importance")
	print("\"fs\": feature search")
	
	if(python_version==2):
		job_type = raw_input("\nPlease choose \"fd\", \"fi\" or \"fs\":\n")
	else:
		job_type = input("\nPlease choose \"fd\", \"fi\" or \"fs\":\n")
	if(job_type not in ["fd","fi","fs"]):
		print("\"" + job_type + "\" is not a valid choice.")
		continue
	else:
		finished=True
		break

def chooseFDJob():
	choosen_fd_job = False
	fd_jobs = []
	allJobs = os.listdir(os.path.join(scenario_dir,"jobs"))
	for job in allJobs:
		if os.path.exists(os.path.join(scenario_dir,"jobs",job,"job.json")):
			with open(os.path.join(scenario_dir,"jobs",job,"job.json")) as f:
				previous_job_data = json.load(f)
			if previous_job_data["type"] == "featureDist":
				fd_jobs.append(job)

	if len(fd_jobs):
		finished = False
		while not finished:
			print("\nPlease choose a feature distribution job. The options are: ")
			for item in fd_jobs:
				print(item)
			print("(Type 'NONE' to create a feature importance job that doesn't use output from a feature distribution job)")
			if python_version == 2:
				choosen_fd_job = raw_input("Choose job: ")
			else:
				choosen_fd_job = input("Choose job: ")
			print("Chosen feature distribution job: "+choosen_fd_job)
			if choosen_fd_job == "NONE":
				choosen_fd_job = False
				finished = True
			elif choosen_fd_job in fd_jobs:
				finished = True
			else:
				print("Invalid choice")
	else:
		print("No fd jobs have previously been run.")
	return(choosen_fd_job)
	
job_data = dict()
if job_type == "fd":
	job_data["type"] = "featureDist"
	print("\nFeature dist job instructions created")
elif job_type == "fi":
	job_data["type"] = "featureImportance"

	finished = False
	while not finished:
		 print("You have selected to create instructions for a feature importance job.")
		 print("Use the results of a feature distribution job as input?")
		 if python_version == 2:
		 	yn = raw_input("\nPlease choose \"y\", or \"n\":\n")
		 else:
		 	yn = input("\nPlease choose \"y\", or \"n\":\n")
		 
		 if yn == "y":
		 	chosenJob = chooseFDJob()
		 	if chosenJob:
		 		job_data["featureDistributionPath"] = chosenJob
		 	finished=True
		 	break
		 elif yn=="n":
		 	finished=True
		 	break
		 else:
		 	print("\nInvalid choice\n")

elif job_type == "fs":
	job_data["type"] = "featureSearch"
	print("\nFeature search job instructions created")
	print("Specify target values in " + os.path.join(scenario_dir,"jobs",job_name,"job.json"))


with open(os.path.join(job_dir,"job.json"),"w") as out_f:
		json.dump(job_data,out_f,indent=4)

print("When ready, use \"python study_simulator.py "+ scenario_name + " " + job_name + "\" to run this job")