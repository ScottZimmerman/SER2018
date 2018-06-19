args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2){
	print("USAGE: Analyze_Results.R <data_dir> <job_dir>")
	q("no")
}
print(args)

data_dir <- args[1]
job_dir <- args[2]

input_folder <- file.path("data",data_dir,"jobs",job_dir)
input_json <- "instructions.json"
scenario_folder <- input_folder
feature_importance_folder <- file.path(input_folder,"featureImportance")
dir.create(feature_importance_folder)

library("rjson")
library("randomForest")
# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------
inputFolder <- file.path(input_folder)
inputs <- fromJSON(file=file.path(inputFolder,input_json))

metric <- NA
tieThreshold <- NA
features_and_performance <- NA
featureImportance_features <- names(inputs$features)
analysisMethods <- inputs$methods

#----------------------------------------
# FEATURE IMPORTANCE
#----------------------------------------
loadFeaturesAndPerformance <- function(path){
	features_and_performance <<- read.csv(path)
}

# Part 1: Classification
# 	Classify each method according to whether it "wins" 
# 	(i.e. is within the tie threshold of the best-performing method for the causal system)
classifyPerformance <- function(){
	tempTable <- abs(features_and_performance[,paste0(names(analysisMethods),"_",metric)])
	bestPerf <- apply(tempTable,1,min)
	classified <- 1*(tempTable < bestPerf+tieThreshold)
	colnames(classified) <- paste0("class_",colnames(classified))
	features_and_performance <<- cbind(features_and_performance,classified)
	print(head(features_and_performance))
}

# Part 2: For each method, apply random forest to get the feature importance for predicting whether an analysis method "wins"
featureImportances <- list()
getFeatureImportance <- function(method,training_features, test_features, training_classes,test_classes){
	
	possibleClasses <- unique(c(training_classes,test_classes))
	
	if(length(possibleClasses) == 1){
		print(paste0("Skipping feature importance for ",method,": Only one class"))
	}else{
		model <- randomForest(
			x=training_features,
			y=factor(training_classes,levels=possibleClasses),
			xtest=test_features,
			ytest=factor(test_classes,levels=possibleClasses),
			importance=TRUE,
			keep.forest=FALSE
		)
		featureImportances[[method]] <<- importance(model)

	}
}

getAllFeatureImportances <- function(includeUnadjusted){
	n <- nrow(features_and_performance)
	test_proportion <- 0.1
	test_n <- round(test_proportion*n)
	test_indices <- sample(1:n,test_n)
	train_indices <- setdiff(1:n,test_indices)

	fiMethods <- names(analysisMethods)
	if(includeUnadjusted==0){
		fiMethods <- fiMethods[fiMethods!="unadjusted"]
	}

	for(method in fiMethods){
		print(paste0("calculating feature importance for ",method))
		method_metric_name <- paste0(method,"_",metric)
		classification_name <- paste0("class_",method_metric_name)
		training_features <- features_and_performance[train_indices,featureImportance_features]
		test_features <- features_and_performance[test_indices,featureImportance_features]
		training_classes <- features_and_performance[train_indices,classification_name]
		test_classes <- features_and_performance[test_indices,classification_name]

		getFeatureImportance(method,training_features, test_features, training_classes, test_classes)
	}
}

#Part 3: Create the summary measure by combining across the feature importances
summaryFeatureImportances <- list()

combined_MDA <- NA
combined_MDG <- NA

summary_MSA <- NA
summary_MSG <- NA

combined_MDA_scaled <- NA
combined_MDG_scaled <- NA

summary_MDA_scaled <- NA
summary_MDG_scaled <- NA

combineFeatureImportances <- function(){
	combined_MDA <<- data.frame(matrix(nrow=length(featureImportance_features),ncol=length(featureImportances)))
	combined_MDG <<- data.frame(matrix(nrow=length(featureImportance_features),ncol=length(featureImportances)))
	
	colnames(combined_MDA) <<- names(featureImportances)
	rownames(combined_MDA) <<- featureImportance_features
	colnames(combined_MDG) <<- names(featureImportances)
	rownames(combined_MDG) <<- featureImportance_features

	for(method in names(featureImportances)){
		combined_MDA[,method] <<- featureImportances[[method]][,"MeanDecreaseAccuracy"]
		combined_MDG[,method] <<- featureImportances[[method]][,"MeanDecreaseGini"]
	}

	summary_MDA <<- cbind(apply(combined_MDA,1,max),apply(combined_MDA,1,sum),apply(combined_MDA,1,mean))
	summary_MDG <<- cbind(apply(combined_MDG,1,max),apply(combined_MDA,1,sum),apply(combined_MDA,1,mean))
	colnames(summary_MDA) <<- c("max","sum","mean")
	rownames(summary_MDA) <<- featureImportance_features
	colnames(summary_MDG) <<- c("max","sum","mean")
	rownames(summary_MDG) <<- featureImportance_features
	

	combined_MDA_scaled <<- NA
	combined_MDG_scaled <<- NA

	summary_MDA_scaled <<- NA
	summary_MDG_scaled <<- NA
	
	print(combined_MDA)
	print(combined_MDG)

	combined_MDA_scaled <<- t(t(combined_MDA)/apply(combined_MDA,2,max))
	combined_MDG_scaled <<- t(t(combined_MDG)/apply(combined_MDG,2,max))

	colnames(combined_MDA_scaled) <<- names(featureImportances)
	rownames(combined_MDA_scaled) <<- featureImportance_features
	colnames(combined_MDG_scaled) <<- names(featureImportances)
	rownames(combined_MDG_scaled) <<- featureImportance_features

	
	print(combined_MDA_scaled)
	print(combined_MDG_scaled)
	
	summary_MDA_scaled <<- cbind(apply(combined_MDA_scaled,1,max),apply(combined_MDA_scaled,1,sum),apply(combined_MDA_scaled,1,mean))
	summary_MDG_scaled <<- cbind(apply(combined_MDG_scaled,1,max),apply(combined_MDG_scaled,1,sum),apply(combined_MDG_scaled,1,mean))
	
	rownames(summary_MDA_scaled) <<- featureImportance_features
	rownames(summary_MDG_scaled) <<- featureImportance_features
	colnames(summary_MDA_scaled) <<- c("max","sum","mean")
	colnames(summary_MDG_scaled) <<- c("max","sum","mean")
}

saveFeatureImportances <- function(folderPath){
	print("SAVING")
	for(method in names(featureImportances)){
		write.csv(featureImportances[[method]],file.path(folderPath,paste0(method,".csv")))
	}	
	write.csv(combined_MDA,file.path(folderPath,paste0(metric,"_MDA_combined.csv")))
	write.csv(combined_MDG,file.path(folderPath,paste0(metric,"_MDG_combined.csv")))
	write.csv(summary_MDA,file.path(folderPath,paste0(metric,"_MDA_summary.csv")))
	write.csv(summary_MDG,file.path(folderPath,paste0(metric,"_MDG_summary.csv")))
	
	write.csv(combined_MDA_scaled,file.path(folderPath,paste0(metric,"_MDA_combined_scaled.csv")))
	write.csv(combined_MDG_scaled,file.path(folderPath,paste0(metric,"_MDG_combined_scaled.csv")))
	write.csv(summary_MDA_scaled,file.path(folderPath,paste0(metric,"_MDA_summary_scaled.csv")))
	write.csv(summary_MDG_scaled,file.path(folderPath,paste0(metric,"_MDG_summary_scaled.csv")))
}

#----------------------------------------
# Additional Feature Calculation
#----------------------------------------
calculateAdditionalFeatures <- function(){
	namesFP <- names(features_and_performance)
	namesCalib <- c() 
	
	for(i in names(calibrationFeatures)){
		namesCalib <- c(namesCalib,paste0(i,"_",c(0,1,2)))
	}

	nCalibFeatures <- 12
	features_and_performance <<- cbind(features_and_performance,data.frame(matrix(nrow=nrow(features_and_performance),ncol=nCalibFeatures)))
	
	names(features_and_performance) <<- c(namesFP,namesCalib)
	
	for(row_index in 1:nrow(features_and_performance)){
		if(row_index %% 10 == 0){
			print(summary(features_and_performance))
			write.csv(features_and_performance,file.path("parallel_test_combined_with_calib.csv"))
			print(row_index)

		}
		seed <- features_and_performance[row_index,"seed"]
		set.seed(seed)
		makeParametricSCM_noLoop(seed)
		measureCalibrationFeatures()
		
		calibFeaturesRow <- data.frame(matrix(nrow=1,ncol=nCalibFeatures))
		i <- 1
		for(featureName in names(calibrationFeatures)){
			calibFeaturesRow[,i:(i+2)] <- calibrationFeatures[[featureName]]
			i <- i + 3
		}
		names(calibFeaturesRow) <- namesCalib
		features_and_performance[row_index,] <- cbind(features_and_performance[row_index,namesFP],calibFeaturesRow)
	}
	print(head(features_and_performance))
	print(summary(features_and_performance))
	write.csv(features_and_performance,file.path("parallel_test_combined_with_calib.csv"))
			
}

saveCorrelations <- function(savePath){
	cors <- cor(head(features_and_performance[,names(inputs$features)]))
	write.csv(cors,file.path(savePath,"feature_correlations.csv"))
}

runLinearModels <- function(){
	features_and_performance_scaled <- features_and_performance
	features_and_performance_scaled[,names(inputs$features)] <-scale(features_and_performance_scaled[,names(inputs$features)])

	for(method in names(analysisMethods)){
		print(method)
		method_metric_name <- paste0(method,"_",metric)
		classification_name <- paste0("class_",method_metric_name)
		formula_string <- paste0(classification_name,"~(",paste0(names(inputs$features),collapse="+"),")*(",paste0(names(inputs$features),collapse="+"),")")
		#formula_string <- paste0(classification_name,"~",paste0(names(inputs$features),collapse="+"))
		m <- glm(formula=as.formula(formula_string),data=features_and_performance_scaled,family=binomial(link='logit'))
		print(m)
	}
}

#----------------------------------------
# WORKSPACE
#----------------------------------------
#---------Feature Importance
loadFeaturesAndPerformance(file.path(feature_importance_folder,"features_and_performance.csv"))
features_and_performance <- abs(features_and_performance)

#MSE
featureImportances <<- list()
metric <<- "MSE"
tieThreshold <<- 0.0001
classifyPerformance()
runLinearModels()
includeUnadjusted <- 0
if(length(names(analysisMethods)==2)){
	includeUnadjusted <- 1
}
getAllFeatureImportances(includeUnadjusted)
combineFeatureImportances()
saveFeatureImportances(file.path(feature_importance_folder))
saveCorrelations(file.path(feature_importance_folder))

#PercBias

featureImportances <<- list()
metric <<- "PercBias"
tieThreshold <<- 0.01
classifyPerformance()
includeUnadjusted <- 0
if(length(names(analysisMethods)==2)){
	includeUnadjusted <- 1
}
getAllFeatureImportances(includeUnadjusted)
combineFeatureImportances()
saveFeatureImportances(file.path(feature_importance_folder))
saveCorrelations(file.path(feature_importance_folder))


# if("featureImportance" %in% names(inputs)){
# 	for(fiSpecName in names(inputs$featureImportance)){

# 		featureImportances <<- list()
# 		metric <<- inputs$featureImportance[[fiSpecName]]$metric
# 		tieThreshold <<- inputs$featureImportance[[fiSpecName]]$tieThreshold
# 		classifyPerformance()
# 		getAllFeatureImportances(0)
# 		combineFeatureImportances()
# 		saveFeatureImportances(file.path(feature_importance_folder),fiSpecName)
# 	}
# }else{
# 	for(fiSpecName in names(inputs$features)){
# 		featureImportances <<- list()
# 		metric <<- "MSE"
# 		tieThreshold <<- 0.0001
# 		classifyPerformance()
# 		includeUnadjusted <- 0
# 		if(length(names(analysisMethods)==2)){
# 			includeUnadjusted <- 1
# 		}
# 		getAllFeatureImportances(includeUnadjusted)
# 		combineFeatureImportances()
# 		saveFeatureImportances(file.path(feature_importance_folder),fiSpecName)
# 	}
# }