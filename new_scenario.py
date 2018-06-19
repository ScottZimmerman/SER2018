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
if(len(sys.argv) != 2):
	print("Usage: python %s <scenario_name>" % sys.argv[0])
	exit()
python_version = sys.version_info[0]

scenario_name = sys.argv[1]
scenario_dir = os.path.join("data",scenario_name)

if not os.path.exists(scenario_dir):
	os.mkdir(scenario_dir)
	os.mkdir(os.path.join(scenario_dir,"jobs"))
	os.mkdir(os.path.join(scenario_dir,"instructions"))
	
	print("Created scenario '" + scenario_name + "'")

	if(python_version==2):
		DGS_script = raw_input("\n[OPTIONAL] Input name of data-generating system (or leave blank for default)\n")
	else:
		DGS_script = input("\n[OPTIONAL] Input name of data-generating system (or leave blank for default)\n")

	if(len(DGS_script)):
		DGS = dict()
		DGS["name"] = DGS_script
		with open(os.path.join(scenario_dir,"instructions","dgs.json"),"w") as f:
			json.dump(DGS,f,indent=4)

else:
	print("Error creating new scenario:")
	print("A Scenario named '"+scenario_name+"' already exists")
	print("Please choose a different name")