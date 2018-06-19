library("rjson")
args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Plot_FS_Results.R <data_dir> <job_dir>")
	q("no")
}

data_dir <- args[1]
job_dir <- args[2] 

instructions <- fromJSON(file=file.path("data",data_dir,"jobs",job_dir,"instructions.json"))
job <- fromJSON(file=file.path("data",data_dir,"jobs",job_dir,"job.json"))

nMethods <- length(instructions$methods)
nTargets <- length(job$targets)

#Feature_Search.R finds a number of causal systems. It attempts to match the values of the features desired by the user.

#Tabulate the classified results

clasified_results_dir <- file.path("data",data_dir,"jobs",job_dir,"featureSearch","classified")
classified_results_files <- list.files(clasified_results_dir)

classified_results_table_MSE <- data.frame(matrix(ncol=nMethods+nTargets, nrow=length(classified_results_files)))
colnames(classified_results_table_MSE) <- c(names(job$targets),names(instructions$methods))
rownames(classified_results_table_MSE) <- classified_results_files

classified_results_table_PercBias <- data.frame(matrix(ncol=nMethods+nTargets, nrow=length(classified_results_files)))
colnames(classified_results_table_PercBias) <- c(names(job$targets),names(instructions$methods))
rownames(classified_results_table_PercBias) <- classified_results_files


for(resultFile in classified_results_files){

	classified_results <- fromJSON(file=file.path(clasified_results_dir,resultFile))

	for(method in names(instructions$methods)){
		classified_results_table_MSE[resultFile,method] <- classified_results$classified[[paste0(method,"_","MSE")]]
		classified_results_table_PercBias[resultFile,method] <- classified_results$classified[[paste0(method,"_","PercBias")]]
	}


	for(feature in names(job$targets)){
		classified_results_table_MSE[resultFile,feature] <- classified_results$features[[feature]]
		classified_results_table_PercBias[resultFile,feature] <- classified_results$features[[feature]]
	}

}

print(classified_results_table_MSE)
print(classified_results_table_PercBias)

#Plot the classified results
write.csv(classified_results_table_MSE,file.path("data",data_dir,"jobs",job_dir,"featureSearch","classified_results_table_MSE.csv"))
write.csv(classified_results_table_PercBias,file.path("data",data_dir,"jobs",job_dir,"featureSearch","classified_results_table_PercBias.csv"))

#Summary tables
# These give the average feature values of the targeted variables (to help assess how well they fit)
# and the proportion of the cases in which each method wins according to the classification criterion
summary_MSE <- apply(classified_results_table_MSE,2,mean)
summary_PercBias <- apply(classified_results_table_PercBias,2,mean)

write.csv(summary_MSE,file.path("data",data_dir,"jobs",job_dir,"featureSearch","summary_MSE.csv"))
write.csv(summary_PercBias,file.path("data",data_dir,"jobs",job_dir,"featureSearch","summary_PercBias.csv"))

print(summary_MSE)