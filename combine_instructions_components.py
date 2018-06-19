import sys
import os
import json

def combine_instructions_components(data_dir,job_dir):
	data_dir = sys.argv[1]
	job_dir = sys.argv[2]
	
	instructions_dir = os.path.join("data",data_dir,"instructions")


	fileNames = ["features","methods","options","TargetParameter"]

	#First check which DGS to use
	dgs_filename = os.path.join(instructions_dir,"dgs.json")
	if os.path.exists(dgs_filename):
		fileNames += ["dgs","parameterRanges"]
	else:
		fileNames += ["DAG","models"]
	
	instructions = dict()

	for fileName in fileNames:
		with open(os.path.join(instructions_dir,fileName+".json")) as f:
			instructions[fileName] = json.load(f)

	with open(os.path.join("data",data_dir,"jobs",job_dir,"instructions.json"),"w") as out_f:
		json.dump(instructions,out_f,indent=4)