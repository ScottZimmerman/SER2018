# Keep track of how well we can find results, and how consistent the recommendations are,
# based on varying numbers of features
featureTargetControl <- list(
	importance = list(),
	fit=NA,
	control = NA,
	consistency = NA,
	currentRow = NA,
	sensitivity = NA
)

setupFeatureTargets <- function(saveFolder){
	
	suppressWarnings(dir.create(file.path(saveFolder,"consistency")))
	suppressWarnings(dir.create(file.path(saveFolder,"consistency_combined")))

	for(item in inputs$featureImportanceResult){
		featureTargetControl$importance[item$key] <<- item$max
	}
	featureTargetControl$consistency <<- matrix(1,nrow=length(inputs$featureTargets), ncol=length(inputs$featureTargets))
	featureTargetControl$consistency[upper.tri(featureTargetControl$consistency)] <<- 0
	featureTargetControl$consistency <<- as.data.frame(featureTargetControl$consistency)
	names(featureTargetControl$consistency) <<- names(featureTargetControl$importance)[which(names(featureTargetControl$importance) %in% names(inputs$featureTargets))]
	
	
	featureTargetControl$control <<- featureTargetControl$consistency  
	featureTargetControl$fit <<- featureTargetControl$consistency
	featureTargetControl$sensitivity <<- featureTargetControl$consistency

	featureTargetControl$currentRow <<- sample(1:nrow(featureTargetControl$control),1)

	featuresTemp <- data.frame(matrix(NA,nrow=nrow(featureTargetControl$control),ncol=ncol(featureTargetControl$control)))
	names(featuresTemp) <- names(featureTargetControl$control)

	#Additional columns for fit: each feature and total
	featuresTempFit <- featuresTemp
	names(featuresTempFit) <- paste0(names(featuresTemp),"_fit")
	featureTargetControl$fit <<- cbind(featureTargetControl$fit,featuresTempFit)
	featureTargetControl$fit$total_fit <<- rep(NA,nrow(featureTargetControl$fit))

	#Additional columns for sensitivity: each feature
	featureTargetControl$sensitivity <<- cbind(featureTargetControl$sensitivity,featuresTemp)
		
	#Additional columns for consistency: each method and total
	methodsTemp <- data.frame(matrix(NA,nrow=nrow(featureTargetControl$control),ncol=length(inputs$methods)+1))
	names(methodsTemp) <- c(names(inputs$methods),"total")
	featureTargetControl$consistency <<- cbind(featureTargetControl$consistency,methodsTemp)

	for(i in 1:nrow(featureTargetControl$consistency)){
		suppressWarnings(dir.create(file.path(saveFolder,"consistency",i)))
	}
}

getFeatureTargets <- function(){
	featureTargetControl$currentRow <<- (featureTargetControl$currentRow)%%nrow(featureTargetControl$control)+1
	print("currentRow")
	print(featureTargetControl$currentRow)
	featuresToUse <- names(featureTargetControl$control)[featureTargetControl$control[featureTargetControl$currentRow,]==1]
	return(inputs$featureTargets[featuresToUse])
}

classify_performance <- function(perf,methods,metric,tieThreshold){
	tempTable <- abs(perf[,paste0(methods,"_",metric)])
	bestPerf <- apply(tempTable,1,min)
	classified <- 1*(tempTable < bestPerf+tieThreshold)
	colnames(classified) <- methods
	
	return(classified)
}

measure_consistency <- function(saveFolder){
	print("measuring consistency")
	for(rowIndex in 1:nrow(featureTargetControl$control)){
		print(paste0("Row Index: ",rowIndex))
		#count performance files
		basePath <- file.path(saveFolder,"consistency",rowIndex)
		results <- list.files(basePath)
		nResults <- length(results)/2

		print(nResults)
		if(nResults > 10){
			performance_results <- NA
			feature_results <- NA

			#load performance and features
			for(item in results){
				if(strsplit(item,"_")[[1]][[1]]=="performance"){
					if(is.na(performance_results)){
						performance_results <- read.csv(file.path(basePath,item))
					}else{
						performance_results <- rbind(performance_results,read.csv(file.path(basePath,item)))
					}
				}
				else if(strsplit(item,"_")[[1]][[1]]=="featuresDist"){
					if(is.na(feature_results)){
						feature_results <- read.csv(file.path(basePath,item))
					}else{
						feature_results <- rbind(feature_results,read.csv(file.path(basePath,item)))
					}
				}

			}

			#Order and clear
			features_and_performance <- sqldf("select * from feature_results as f join performance_results as p on f.seed=p.seed")
			features_and_performance[,which(names(features_and_performance)=="seed")[[2]]]<- NULL
			performance_results <- NULL
			feature_results <- NULL

			if(!("Conf_over_ATE" %in% names(features_and_performance))){
				features_and_performance$Conf_over_ATE <- features_and_performance$Conf/features_and_performance$ATE
			}

			#Classify performance
			classified <- classify_performance(features_and_performance,names(inputs$methods),"MSE",0.0001)
			
			features_and_performance <- cbind(features_and_performance,classified)
			
			#Assess fit
			featuresToUse <- names(featureTargetControl$control)[featureTargetControl$control[rowIndex,]==1]
			
			fits <- data.frame(matrix(nrow=nrow(features_and_performance),ncol=(length(featuresToUse))))
			colnames(fits) <- paste0(featuresToUse,"_fit")
		
			for(fName in featuresToUse){
				fits[,paste0(fName,"_fit")] <- abs((features_and_performance[,fName]-inputs$featureTargets[[fName]])/inputs$featureTargets[[fName]])
			}

			fits$total_fit <- apply(fits,1,sum)
			features_and_performance <- cbind(features_and_performance,fits)		

			featureTargetControl$fit[rowIndex,paste0(c(featuresToUse,"total"),"_fit")] <<- apply(fits[,paste0(c(featuresToUse,"total"),"_fit")],2,mean)				

			#Limit to well-fit only
			cutoff <- length(featuresToUse)*0.05
			goodFits <- features_and_performance[which(abs(features_and_performance$total_fit)<cutoff),]

			goodFits_Min_N <- 10
			if(nrow(goodFits) > goodFits_Min_N){

				#Assess consistency
				classified_consistency <- 2*abs(0.5-apply(goodFits[,names(inputs$methods)],2,mean))
				total_consistency <- mean(classified_consistency)
				
				featureTargetControl$consistency[rowIndex,names(inputs$methods)] <<- classified_consistency
				
				featureTargetControl$consistency[rowIndex,"total"] <<- total_consistency
				
			}
			#Assess sensitivity
			# TO DO: needs to have some variation in fit to work (could target slight variations, or perturb the CSs)

		}
	}
	baseDir <- file.path(saveFolder,"consistency_combined")
	write.csv(featureTargetControl$control,file.path(baseDir,"control.csv"))
	write.csv(featureTargetControl$fit,file.path(baseDir,"fit.csv"))
	write.csv(featureTargetControl$consistency,file.path(baseDir,"consistency.csv"))
	write.csv(featureTargetControl$sensitivity,file.path(baseDir,"sensitivity.csv"))
}