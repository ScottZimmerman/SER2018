args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Classify.R <data_dir> <job_dir>")
	q("no")
}

data_dir <- args[1]
job_dir <- args[2]

library("rjson")

# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------
input_dir <- file.path("data",data_dir,"jobs",job_dir)
feature_search_dir <- file.path(input_dir,"featureSearch")

targeted_results_dir <- file.path(feature_search_dir,"targeted")
classified_results_dir <- file.path(feature_search_dir,"classified")

dir.create(classified_results_dir)

inputs <- fromJSON(file=file.path(input_dir,"instructions.json"))

tieThresholds <- list(
	"MSE"=0.0001,
	"PercBias"=0.01
)

analysisMethods <- inputs$methods
methodNames <- names(analysisMethods)

#----------------------------------------
# CLASSIFICATION
#----------------------------------------
# 	Classify each method according to whether it "wins" 
# 	(i.e. is within the tie threshold of the best-performing method for the causal system)
classifyPerformance <- function(result,metric){
	tempTable <- abs(as.data.frame(result$performance)[,paste0(methodNames,"_",metric)])
	bestPerf <- apply(tempTable,1,min)
	classified <- 1*(tempTable < bestPerf+tieThresholds[[metric]])
	return(classified)
}

#----------------------------------------
# WORKSPACE
#----------------------------------------
#Download files
print("MONGO WAS CALLED HERE")
#system(paste0(c("python","download_targeted_results_mongodb.py",mongo_config_path, data_dir),collapse=" "))
resultsFiles <- list.files(targeted_results_dir)

#Process and upload files
for(fileName in resultsFiles){
	result <- fromJSON(file=file.path(targeted_results_dir,fileName))
	classified <- NA
	for(metric in names(tieThresholds)){
		if(is.na(classified)){
			classified <- classifyPerformance(result, metric)	
		}else{
			classified <- cbind(classified,classifyPerformance(result, metric))
		}
	}
	result$classification_thresholds <- tieThresholds 
	result$classified <- as.list(classified[1,])
	write(toJSON(result),file.path(classified_results_dir,fileName))

	print("MONGO WAS CALLED HERE")
	#system(paste0(c("python","update_targeted_result_mongodb.py",mongo_config_path,data_dir,fileName),collapse=" "))
}