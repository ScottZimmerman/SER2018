args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Compile_Results.R <data_dir> <job_dir>")
	q("no")
}

data_dir <- args[1]
job_dir <- args[2]

results_folder <- file.path("data",data_dir,"jobs",job_dir)
feature_importance_folder <- file.path(results_folder,"featureImportance")
dir.create(feature_importance_folder)

library("sqldf")

resultsDir <- file.path(results_folder,"raw")
fileNames <- list.files(resultsDir)

#Create performance df by combining individual files
performance_df <- NA

#create features df
features_df <- NA

#Fill in these data frames
for(fileName in fileNames){
	prefix <- strsplit(fileName,"_")[[1]][[1]]
	if(prefix == "performance"){
		tryCatch({
			if(suppressWarnings(is.na(performance_df))){
				performance_df <- read.csv(file.path(resultsDir,fileName))
			}else{
				performance_df <- rbind(performance_df,read.csv(file.path(resultsDir,fileName)))
			}
		},error=function(e){
			print(paste0("couldn't get ",fileName))
		})
	}
	if(prefix == "featuresDist"){
		tryCatch({
			if(suppressWarnings(is.na(features_df))){
				features_df <- read.csv(file.path(resultsDir,fileName))
			}else{
				features_df <- rbind(features_df,read.csv(file.path(resultsDir,fileName)))
			}
		},error=function(e){
			print(paste0("couldn't get ",fileName))
		})
	}
}

write.csv(features_df,file.path(feature_importance_folder,"features.csv"))
write.csv(performance_df,file.path(feature_importance_folder,"performance.csv"))

#join based on seed
performance_df[,"ATE"] <- NULL
query <-"select * from (select * from performance_df) as p join (select * from features_df) as f on f.seed=p.seed"
combined <- sqldf(query)
seedIndex2 <- which(names(combined) == "seed")[[2]]
combined[,seedIndex2] <- NULL

write.csv(combined,file.path(feature_importance_folder,"features_and_performance.csv"))
