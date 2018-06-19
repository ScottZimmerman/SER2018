args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Plot_FI_Results.R <data_dir> <job_dir>")
	q("no")
}


data_dir <- args[1]
job_dir <- args[2] 

job_path <- file.path("data",data_dir,"jobs",job_dir)
MSE_MDA_summary_scaled <- read.csv(file.path(job_path,"featureImportance","MSE_MDA_summary.csv"))
print(MSE_MDA_summary_scaled)