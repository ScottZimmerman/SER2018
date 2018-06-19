args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Feature_Distribution.R <data_folder> <job_dir>")
	q("no")
}
data_folder <- file.path("data",args[1])
job_folder <- file.path(data_folder,"jobs",args[2])

library("rjson")
library("sqldf")
library("psych")

inputs <- fromJSON(file=file.path(job_folder,"instructions.json"))
job_info <- fromJSON(file=file.path(job_folder,"job.json"))
dir.create(file.path(job_folder,"output"), showWarnings=FALSE)
dir.create(file.path(job_folder,"output","featuresByThetas_Filtered"), showWarnings=FALSE)

featureRanges <- list(
	ATE=list(min=-1,max=1),
	OF=list(min=0,max=1),
	MinTxProp=list(min=0,max=1),
	Conf_over_ATE=list(min=-1,max=1)
)
saveInterval <- 100
plot <- 0
plotInterval <- 10
maxReps <- inputs$options$maxReps_featureDist
startFeatureImportanceIndex <- inputs$options$startFeatureImportanceIndex

if(saveInterval > maxReps){
	saveInterval <- maxReps
}

source("Simulate.R")
thetaDF <- data.frame(matrix(nrow=0, ncol=(nNonFixedParams))) 
featuresDF <- data.frame(matrix(nrow=0, ncol=(length(featureRanges)))) 
names(thetaDF) <- paste0("theta_",1:nNonFixedParams)
names(featuresDF) <- names(featureRanges)


nPtsDensityCutoff <- 1
densityRadii <- list()
nBurnIn <- 1
nAttempts_update_threshold <- 100
nPtsDensityCutoff <- 1
paramValues <- list(
	densityScale = 0.25,
	lastFindThetasUpdate=1
)
featureDistribution <- data.frame(matrix(nrow=0,ncol=length(featureRanges)))
names(featureDistribution) <- names(featureRanges)

updateDensityRadii <- function(scale){
	print(paste0("Changing density scale to ",scale))
	for(fr in names(featureRanges)){
		densityRadii[fr] <<- scale*(featureRanges[[fr]]$max-featureRanges[[fr]]$min)
	}
}
updateDensityRadii(paramValues$densityScale)

# Attempt at a better sampling strategy.
probabilitySampleFilter <- function(targets){
	accepted <- 1
	ta <- as.data.frame(targets)[,names(featureDistribution)]

	if(nrow(featureDistribution) > nBurnIn){
		#Acceptance probability is higher if the region is sparse
		whereItems <- c()
		for(di in names(densityRadii)){
			whereItems <- c(whereItems, paste0(di,">",targets[[di]]-densityRadii[[di]], " and ", di,"<",targets[[di]]+densityRadii[[di]]))
		}
		whereString <- paste0(whereItems,collapse=" and ")

		query <- paste0("select count(*) as count from featureDistribution where ",whereString)
		numPts <- sqldf(query)$count
		highDensity <- (numPts > nPtsDensityCutoff)
		if(highDensity){
			accepted <- 0
		}
	}

	if(accepted){
		featureDistribution <<- rbind(featureDistribution,ta)
	}	
	return(accepted)
}

getTheta <- function(){
	theta <- c()
	for(var in names(pXgivenPa)){
		theta <- c(theta,pXgivenPa[[var]]$P)
	}
	return(theta)
}
seeds <- c()
setupFeatureHandlers()
for(i in 1:maxReps){
	seed <- floor(1000000000*runif(1))
	seeds <- c(seeds,seed)
	set.seed(seed)
	makeParametricSCM_noLoop(seed)
	measureFeatures()

	features$Conf_over_ATE <- features$Conf/features$ATE
	
	accepted <- 0
	nAttempts <- 0
		
	while(!accepted){
		targets <- as.data.frame(features)[,c("ATE","OF","MinTxProp","Conf_over_ATE")]
		accepted <- probabilitySampleFilter(targets)
		cat(".")
			
		nAttempts <- nAttempts + 1
		if(nAttempts > nAttempts_update_threshold){
			nAttempts <- 0
			paramValues$densityScale <- paramValues$densityScale*0.5
			updateDensityRadii(paramValues$densityScale)
		}
	}

	featuresDistRow <- as.data.frame(features)
	print(featuresDistRow)
	featuresDF[i,] <- featuresDistRow[,names(featuresDF)]
	theta <- getTheta()
	thetaDF[i,] <- theta
	write(toJSON(list(seed=seed,theta=theta,features=featuresDF[i,])),file.path(job_folder,"output","featuresByThetas_Filtered",paste0(seed,".json")))

	if(plot){
		if(i%%plotInterval==0){
			png(file.path(job_folder,"features_by_random_theta_filtered.png"))
			tempTable <- featuresDF[,c("ATE","OF","MinTxProp","Conf_over_ATE")]
			tempTable <- sqldf("select * from tempTable where Conf_over_ATE < 1 and Conf_over_ATE > -1")
			pairs.panels(tempTable)
			dev.off()
		}
	}
	if(i%%saveInterval==0){
		print("MONGO WAS CALLED HERE")
		#system(paste0(c("python",file.path("save_thetaAndFeatures_mongoDB.py"),mongo_config_path,data_folder,paste0(seeds,collapse=",")),collapse=" "))
		
		write.csv(cbind(thetaDF,featuresDF),file.path(job_folder,"featuresByThetas_filtered.csv"))
		seeds <- c()
	}

	if(i == startFeatureImportanceIndex){
		print("MONGO WAS CALLED HERE")
		#system(paste0(c("python",file.path("create_job.py"),mongo_config_path,job_info$user,job_info$instructions_collection, job_info$instructions_id,"featureImportance"),collapse=" "))
	}
}

