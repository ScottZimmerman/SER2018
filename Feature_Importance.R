args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Feature_Importance.R <data_dir> <job_dir>")
	q("no")
}

print(args)
data_dir <- file.path("data",args[1])
job_dir <- args[2] 

input_folder <- file.path(data_dir,"jobs",job_dir)
input_json <- file.path(input_folder,"instructions.json")
output_folder <- file.path(data_dir,"jobs",job_dir)

library("rjson")
library("stringr")

# ------------------------------------------------------------
# Input Files
# ------------------------------------------------------------
inputFolder <- file.path(input_folder)
inputs <- fromJSON(file=file.path(input_json))
job <- fromJSON(file=file.path(inputFolder,"job.json"))

print("MONGO WAS CALLED HERE")
#Update job status in DB to "running"
#system(paste0(c("python",file.path("change_job_status.py"), mongo_config_path, job$user, job$id_by_user,"running"),collapse=" "))

source("Simulate.R")
setupHandlers()

#----------------------------------------
# WORKSPACE
#----------------------------------------
calculateFeatureImportance <- function(input_folder,output_folder,saveInterval=1){
	raw_path <-file.path(output_folder,"raw")
	if(!file.exists(raw_path)){
		dir.create(raw_path,recursive=TRUE)
	}
	
	thetaAndFeaturesPath <-file.path(input_folder,"thetaAndFeatures")
	if(!file.exists(thetaAndFeaturesPath)){
		dir.create(thetaAndFeaturesPath,recursive=TRUE)
	}

	featureRepIndex <- 0
	while(!finished){
		if(node_ID=="0" || compute_resource=="desktop"){
			print("NODE ID 0")
			results <- list.files(file.path(output_folder,"raw"))
			nResults <- length(results)/2
			
			print(paste0("nResults: ",nResults))
			if((nResults != 0) && nResults%%featureRepUploadInterval == 0){
				print("Compiling feature importance results")
				system(paste0(c("Rscript",file.path("Compile_Results.R"),args[1],args[2]),collapse=" "))

				print("Analyzing feature importance results")
				system(paste0(c("Rscript",file.path("Analyze_Results.R"),args[1],args[2]),collapse=" "))

				print("MONGO WAS CALLED HERE")
				# if(mongo_config_path != "NONE"){
				# 	print("Uploading feature importance results")
				# 	system(paste0(c("python",file.path("save_featureImportance_mongoDB.py"),mongo_config_path,input_folder,input_json,output_folder),collapse=" "))
				# }
			}
			if(nResults >= nFeaturesReps){
				print("MONGO WAS CALLED HERE")
				print("Compiling feature importance results")
				system(paste0(c("Rscript",file.path("Compile_Results.R"),args[1],args[2]),collapse=" "))

				print("Analyzing feature importance results")
				system(paste0(c("Rscript",file.path("Analyze_Results.R"),args[1],args[2]),collapse=" "))

				# if(mongo_config_path != "NONE"){
				# 	system(paste0(c("python",file.path("change_job_status.py"), mongo_config_path, job$user, job$id_by_user,"finished","1"),collapse=" "))
				# }
				if(compute_resource == "savio"){
					system(paste0("scancel ",Sys.getenv("SLURM_JOB_ID")))	
				}
				print("FINISHED")
				q("no")
			}
		}

		featureRepIndex <- featureRepIndex + 1

		print(paste0("---------------------",featureRepIndex,"---------------------"))
		
		#if(mongo_config_path != "NONE"){
			print("MONGO WAS CALLED HERE")
			#nextTheta <- system(paste0(c("python","get_next_thetaAndFeatures_mongoDB.py",mongo_config_path,input_folder,input_json),collapse=" "),intern=TRUE)
		
		if("featureDistributionPath" %in% names(job)){
			thetaPath <- file.path(data_dir,"jobs",job$featureDistributionPath,"output","featuresByThetas_Filtered")
			allTheta <- list.files(thetaPath)
			allTheta <- str_replace(allTheta,".json","")
			handledTheta <- list.files(raw_path)
			handledTheta <- handledTheta[grepl("^featuresDist",handledTheta)]
			handledTheta <- str_replace(handledTheta,"featuresDist_","")
			handledTheta <- str_replace(handledTheta,".csv","")

			remainingTheta <- setdiff(allTheta,handledTheta)
			if(length(remainingTheta)){
				nextTheta <- file.path(thetaPath,paste0(remainingTheta[[1]],".json"))
			}else{
				nextTheta <- "NONE"
			}
		}else{
			nextTheta <- "NONE"
		}

		if(nextTheta == "NONE"){
			seed <- floor(1000000000*runif(1))
		}else if("featureDistributionPath" %in% names(job)){
			#For running locally
			CS <- fromJSON(file=nextTheta)
			seed <- CS$seed
		}else{
			#Placeholder: nextTheta should be a string, obtained from another source (e.g. S3)
			CS = fromJSON(nextTheta)
			seed <- CS$seed
		}
		set.seed(seed)		
		makeParametricSCM_noLoop(seed)
		measureFeatures()

		featuresDistRow <- as.data.frame(c(list("seed"=seed),features))
		featuresDist[featureRepIndex,] <<- featuresDistRow[,names(featuresDist)]
		runAnalysesManyTimes()
		calculatePerformance(seed,featureRepIndex)

		if(featureRepIndex%%saveInterval == 0){
			saveFeaturesDistribution(raw_path,featureRepIndex,seed)
			savePerformance(raw_path,featureRepIndex,seed)
			uploadResults(seed)
		}
		
	}
	
}
calculateFeatureImportance(input_folder,output_folder)
