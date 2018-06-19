args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Feature_Search.R <data_dir> <job_dir>")
	q("no")
}

print(args)

data_dir <- args[1]
job_dir <- args[2]

input_json <- "instructions.json"

input_folder <- file.path("data",data_dir,"jobs",job_dir)
output_folder<- file.path(input_folder,"featureSearch")

library("rjson")

# ------------------------------------------------------------
# Input Files
# ------------------------------------------------------------
inputFolder <- file.path(input_folder)
inputs <- fromJSON(file=file.path(inputFolder,input_json))
nFeatureSearchReps_max <- inputs$options$nFeatureSearchReps_max
#targetsJSON <- fromJSON(file=file.path(inputFolder,"targets.json"))
job <- fromJSON(file=file.path(inputFolder,"job.json"))
targets <- list()
for(target in names(job$targets)){
	targets[target] <- job$targets[[target]]$value
}

#Update job status in DB to "running"
print("MONGO WAS CALLED HERE")
#system(paste0(c("python",file.path("change_job_status.py"), mongo_config_path, job$user, job$id_by_user,"running"),collapse=" "))

source("Simulate.R")
setupHandlers()

compute_resource <- Sys.getenv("STUDYSIMRESOURCE")	
if(compute_resource!="desktop"){
	node_ID <- Sys.getenv("OMPI_COMM_WORLD_RANK")
}

targeted_path <-file.path(output_folder,"targeted")
if(!file.exists(targeted_path)){
	dir.create(targeted_path,recursive=TRUE)
}

# ------------------------------------------------------------
# Functions
# ------------------------------------------------------------
measureTargetFit <- function(){
	t_DF <- as.data.frame(targets)
	f_DF <- as.data.frame(features)[,names(t_DF)]
	d <- max(abs((t_DF-f_DF)/f_DF))
	return(d)
}

getExistingCSs <- function(){

	print("MONGO WAS CALLED HERE")
	#system(paste0(c("python","getExistingCSs.py",mongo_config_path,data_dir),collapse=" "))
}

finish <- function(status){
	system(paste0(c("Rscript","Classify.R", data_dir, job_dir),collapse=" "))

	print("MONGO WAS CALLED HERE")
	#system(paste0(c("python",file.path("change_job_status.py"), mongo_config_path, job$user, job$id_by_user,status,"1"),collapse=" "))
}
findNewCSs <- function(){
	featureRepIndex <- 1
	NSGA2_Options <- list(
		generations = 16,
		popsize = 32 #popsize must be a multiple of 4
	)
	counter <- 0
	while(!finished){
		
		print(NSGA2_Options)
		CSs <- find_CS_NSGA2(targets,NSGA2_Options)
		anyFound <- 0

		for(parIndex in 1:nrow(CSs$par)){
			theta <- CSs$par[parIndex,]
			
			makeParametricSCM_FromTheta(theta)
			measureFeatures()
			d <- measureTargetFit()
			
			if(d < 0.1){
				counter <- counter + 1
				runAnalysesManyTimes()
				calculatePerformance(NA,featureRepIndex)
				results_filename <- paste0(featureRepIndex,".json")
				#output <- list(targets_id=targetsJSON$id_by_user,theta=theta,features=features,targets=targets,performance=performance[featureRepIndex,])
				output <- list(theta=theta,features=features,targets=targets,performance=performance[featureRepIndex,])
				write(toJSON(output),file=file.path(targeted_path,results_filename))
				anyFound <- 1
				featureRepIndex <- featureRepIndex + 1

				print("MONGO WAS CALLED HERE")
				#system(paste0(c("python","save_targeted_results_mongoDB.py",mongo_config_path,data_dir,results_filename),collapse=" "))
				
				if(counter >= nFeatureSearchReps_max){
					finish("finished")
					q("no")
				}
			}
		}
		if(!anyFound){
			print("None found")

			if(NSGA2_Options$generations < 250 || NSGA2_Options$popsize < 250){
				if(NSGA2_Options$generations >= 250){
					NSGA2_Options$popsize <- NSGA2_Options$popsize*2
				}else if(NSGA2_Options$popsize >= 250){
					NSGA2_Options$generations <- NSGA2_Options$generations*2
				}else{
					if(rbinom(1,1,0.5)){			
						NSGA2_Options$generations <- NSGA2_Options$generations*2
					}else{
						NSGA2_Options$popsize <- NSGA2_Options$popsize*2
					}	
				}
				
			}else{
				finish("failure")
				finished <- 1
				q("no")
			}
		}
	}
}

getExistingCSs()
#TO DO: Count existing, if enough, don't continue.
findNewCSs()