function(inputs){
		
	library("sqldf")
	library("MASS")
	library("ltmle")
	library("rjson")
	library("mco")
	source("measure_consistency.R")
	#When running in parallel, can use the node ID for output labeling
	node_ID <- Sys.getenv("OMPI_COMM_WORLD_RANK")	
	compute_resource <- Sys.getenv("STUDYSIMRESOURCE")	

	# This script can be run in two different ways:
	#	JD Mode: ("Joint Distribution") Uses the full joint distribution of P(X) to calculate probabilities
	#	CD Mode: ("Conditional Distributions") Uses the factorization P(X) = Product_{i}[P(X_i|Pa(X_i))] to calculate probabilities
	#
	#	JD mode invloves an up-front computational cost of creating the joint distribution and will thus be slow (or impossible) if the full factorial combination of possible variable values is large
	#	CD mode computes exact probabilities through recursive application of the conditional law of total probability and the factorization of joint distributions
	#	With very large or complex bayesian networks MCMC sampling will be necessary (not handled by this script)

	finished <- 0
	features <- list()
	
	# ------------------------------------------------------------
	# Global Parameters
	# ------------------------------------------------------------
	probMode <- inputs$options$probMode #"CD" or "JD": which mode to use to calculate probabilities (see notes above)
	nFeaturesReps <- inputs$options$nFeaturesReps
	featureRepUploadInterval <- inputs$options$featureRepUploadInterval
	nEstimateReps <- inputs$options$nEstimateReps
	sampleSize <- inputs$options$sampleSize

	# ------------------------------------------------------------
	# Utility Functions
	# ------------------------------------------------------------
	rowToList <- function(row,curVar){ 
		if(length(row) == 1){
			result <- list()
			result[[curVar]] <- row
		}else{
			result <- as.list(row)
		}
		return(result)
	}

	# ------------------------------------------------------------
	# Bayesian Network Structural Assumptions
	# ------------------------------------------------------------
	parents <- inputs$dag$parents
	for(pa in names(parents)){
		if(!length(parents[[pa]])){
			parents[[pa]] <- c()		
		}
	}
	fullTopoOrder <- inputs$dag$fullTopoOrder
	exposure <- inputs$targetParameter$exposure
	outcome <- inputs$targetParameter$outcome
	confounders <- intersect(parents[[exposure]],parents[[outcome]])

	# ------------------------------------------------------------
	# Bayesian Network Mechanistic Assumptions
	# ------------------------------------------------------------
	posVals <- inputs$dag$posVals

	# ------------------------------------------------------------
	# Study Design/Analysis Methods
	# ------------------------------------------------------------
	modelForms <- inputs$models
	models <- list(
		g=list(),
		Q=list()
	)
	analysisMethods <- inputs$methods

	# ------------------------------------------------------------
	# Features
	# ------------------------------------------------------------
	featureInstructions <- inputs$features

	# ------------------------------------------------------------
	# CACHING
	# ------------------------------------------------------------
	# This section improves performance of feature calculation in CD mode
	# jdCache stores p of joint dists
	# format: jdCache[[nine-digit-string]]
	# The nine-digit-string denotes which table, using the topological ordering
	jdCache <- list()
	makeNDS <- function(varList){
		nds <- paste0(as.character(as.numeric(fullTopoOrder %in% names(varList))),collapse="")
		return(nds)
	}
	getFromCache <- function(nds,varList){
		#nds: nine-digit-string
		query <- makeQuery(varList)
		tempTable <- jdCache[[nds]]
		res <- sqldf(query)
		result <- list()
		if(nrow(res) == 1){
			result$pInChache <- 1
			result$p <- res[,"P"]
		}else if(nrow(res) == 0){
			result$pInChache <- 0
		}else{
			print("Error in getFromCache: returned multiple rows")
			q("no")
		}
		return(result)
	}

	checkCache <- function(varList){
		#varList is variableName: variableValue pairs
		nds <- makeNDS(varList)
		tableInCache <- (nds %in% names(jdCache))
		result <- list(
			tableInCache = tableInCache,
			pInCache = 0,
			p = NA
		)
		if(tableInCache){
			gfc_result <- getFromCache(nds,varList)
			result$pInCache <- gfc_result$pInChache
			if(result$pInCache){
				result$p <- gfc_result$p
			}
		}
		return(result)
	}

	addToCache <- function(varList,p){
		if(checkCache(varList)$pInCache == 0){
			nds <- makeNDS(varList)
			orderedNames <- fullTopoOrder[fullTopoOrder %in% names(varList)]
			if(!(nds %in% names(jdCache))){
				jdCache[[nds]] <<- data.frame(matrix(nrow=0,ncol=(length(varList)+1)))
				names(jdCache[[nds]]) <<- c(orderedNames,"P")
			}
			inputRow <- as.data.frame(c(varList,list(P=p)))
			orderedRow <- inputRow[,names(jdCache[[nds]])]
			jdCache[[nds]] <<- rbind(jdCache[[nds]],orderedRow)
		}
	}

	# ------------------------------------------------------------
	# BAYESIAN NETWORK PARAMETRIC ASSUMPTIONS
	# ------------------------------------------------------------
	# Store the values of P(X|Pa(X))
	pXgivenPa <- list()

	if("fixParametersScript" %in% names(inputs)){
		fixParams <- dget(inputs$fixParametersScript)(inputFolder)
		pXgivenPa <- fixParams(pXgivenPa)
	}

	# Instantiate mechanisms, but do not assign values fo each P(X|Pa(X))
	nonFixedVariables <- setdiff(fullTopoOrder,names(pXgivenPa))
	for(curVar in nonFixedVariables){
		pXgivenPa[[curVar]] <- expand.grid(posVals[c(parents[[curVar]],curVar)])
		pXgivenPa[[curVar]][,"P"] <- NA
		ID <-1:nrow(pXgivenPa[[curVar]])
		pXgivenPa[[curVar]] <- cbind(ID,pXgivenPa[[curVar]])
	}

	# We will be repeatedly simulating different parameterizations of the causal system
	# 	this function will reset the values in between iterations
	resetDist <- function(){
		for(tab in nonFixedVariables){
			pXgivenPa[[tab]][,"P"] <<- 0
		}
	}
	# Determine how many non-fixed parameters of P(X|Pa(X)) the system has
	nParams <- list()
	for(varName in names(pXgivenPa)){
		nParams[[varName]] <- sum(is.na(pXgivenPa[[varName]][,"P"]))
	}

	# Determine the total number of non-fixed parameters in the system
	nNonFixedParams <- sum(as.numeric(nParams))

	#This function parameterizes P(X|Pa(X))
	makeParametricSCM <- function(){
		resetDist()
		for(curVar in names(pXgivenPa)){
			print(curVar)
			if(nParams[[curVar]] > 0){
				tempTable <- pXgivenPa[[curVar]]
				parentsX <- names(pXgivenPa[[curVar]])[!(names(pXgivenPa[[curVar]]) %in% c(curVar,"P","ID"))]
				prevVarValues <- expand.grid(posVals[parentsX])
				
				for(i in 1:nrow(prevVarValues)){
					if(i%%100 == 0){
						print(i)
					}
					queryVars <- c()
					for(j in 1:ncol(prevVarValues)){
						prevName <- names(prevVarValues)[j]
						prevVal <- prevVarValues[i,j]
						
						if(typeof(prevVal) == "character" || is.factor(prevVal)){
							prevVal <- paste0("'",prevVal,"'")
						}
						queryVars <- c(queryVars,paste0(prevName,"=",prevVal))
					}
					queryVars <- paste0(queryVars,collapse=" and ")
					query <- paste0("select ID from tempTable where ",queryVars)
					ids <- as.numeric(sqldf(query)[["ID"]])
					
					#OLD THETA SAMPLING SCHEME: Not ideal, doesn't sample the entire parameter space well. Things end up too uniform.
					#p <- runif(length(ids))
					#p <- p/sum(p)

					#NEW THETA SAMPLING SCHEME:
					# This samples uniformly
					# Create a random ordering of the ids, allocate part of the remaining distribution to each one in turn

					remainingP <- 1
					ID_ordering <- sample(1:length(ids),replace=FALSE)
					randP <- runif(length(ids))
					p <- rep(NA,length(ids))
					for(i in ID_ordering){

					}
					print("makeParametricSCM not updated with new theta sampling scheme")
					q("no")
					
					tempTable[ids,"P"] <- tempTable[ids,"P"] + p
				}
				pXgivenPa[[curVar]] <<- tempTable
			}
		}
		jdPath <- file.path("distributions", folderName,"fullJD.csv")
		if(!file.exists(jdPath)){
			makeJDTable()
		}
	}

	JD_factorize <- function(vals){
		topo <- fullTopoOrder[fullTopoOrder %in% names(vals)]
		factors <- list()
		prevVars <- c()
		for(curVarIndex in 1:length(topo)){
			curVar <- topo[curVarIndex]
			factors[[curVar]] <- list()

			for(prevVar in prevVars){
				factors[[curVar]][[prevVar]] = vals[[prevVar]]
			}

			prevVars <- c(prevVars, curVar)
		}
		return(factors)
	}

	makeQuery <- function(queryVals,selection="*",tableName="tempTable"){
		queryVars <- c()
		for(queryVarName in names(queryVals)){
			queryVarValue <- queryVals[[queryVarName]]
			if(typeof(queryVarValue) == "character" || is.factor(queryVarValue)){
				queryVarValue <- paste0("'",queryVarValue,"'")
			}
			queryVars <- c(queryVars,paste0(queryVarName,"=",queryVarValue))
		}
		queryVars <- paste0(queryVars,collapse=" and ")
		query <- paste0("select ",selection," from ",tableName," where ",queryVars)
		return(query)
	}

	CP_full <- function(LHS,RHS){
		tempTable <- pXgivenPa[[names(LHS)]]
		queryVals <- c(LHS,RHS)
		query <- makeQuery(queryVals)
		res <- sqldf(query)
		p <- res[,"P"]
		if(length(p) > 1){
			print("ERROR: CP_full query wasn't fully specified")
			print(query)
			print(res)
			q("no")
		}
		return(p)
	}

	makeCPString <- function(LHS_vals,RHS_vals){
		LHS <- c()
		for(x in names(LHS_vals)){
			LHS <- c(LHS,paste0(x,"=",LHS_vals[[x]]))
		}
		LHS <- paste0(LHS,collapse=",")
		result <- c("P(",LHS)
		if(length(RHS_vals)){
			RHS <- c()
			for(x in names(RHS_vals)){
				RHS <- c(RHS,paste0(x,"=",RHS_vals[[x]]))
			}
			RHS <- paste0(RHS,collapse=",")
			result <- c(result,"|",RHS,")")
		}else{
			result <- c(result,")")
		}
		result <- paste0(result,collapse="")
		return(result)
	}

	makeJPString <- function(vals){
		return(makeCPString(vals,list()))
	}

	handleFactor <- function(f,vals,curVar){
		prevVals <- c(names(f),curVar)
		curVarParents <- parents[[curVar]]
		expandVars <- curVarParents[!(curVarParents %in% prevVals)]

		if(length(expandVars)){
			expandedGrid <- expand.grid(posVals[expandVars])
			p <- 0
			
			for(egIndex in 1:nrow(expandedGrid)){
				#Applying the conditional law of total probability
				# P(X|Y) = Sum_{z}[P(X|Y,z)P(z|Y)]
				# Let P_1 = P(X|Y,z)
				#     P_2 = P(z|Y)
				
				# expandedGrid holds values of z
				z <- rowToList(expandedGrid[egIndex,],names(expandedGrid))
				x <- list()
				x[[curVar]] <- vals[[curVar]]
				y <- f
				yz <- c(z,y)
				P_1 <- CP(x,yz)
				P_2 <- CP(z,y)
				p <- p + P_1*P_2
			}
		}else{
			LHS <- list()
			LHS[[curVar]] <- vals[[curVar]]
			RHS <- vals[curVarParents]
			p <- CP_full(LHS,RHS)
		}
		return(p)
	}


	JPs <- list(
		JD = function(vals){
			query <- makeQuery(vals,"Sum(P) as Sum_P","fullJD")
			p <- sqldf(query)[1,"Sum_P"]
			return(p)
		},
		CD = function(vals){
			cacheResult <- checkCache(vals)
			if(cacheResult$pInCache == 0){
				#Factorize P(X1,...,Xk) as P(Xk|X_{k-1},...,X1)P(X_{k-1}|X_{k-2},...,X1) ... P(X1)
				factors <- JD_factorize(vals)
				p <- 1
				for(curVar in names(factors)){
					#print(paste0("factor: ",curVar))
					factorP <- handleFactor(factors[[curVar]],vals,curVar)
					p <- p*factorP
				}
			
				#Add to cache
				addToCache(vals,p)
			}else{
				p <- cacheResult$p
			}
			return(p)
		}
	)

	JP <- function(vals){
		return(JPs[[probMode]](vals))
	}

	is_form_pXgivenPa <- function(LHS_vals, RHS_vals){
		full <- 0
		if(length(LHS_vals) == 1){
			par <- parents[[names(LHS_vals)[[1]]]]
	 		if(length(par) == length(RHS_vals)){
	 			if(prod(par %in% names(RHS_vals))){
	 				full <- 1
	 			}
	 		}
		}
		return(full)
	}

	cond_LTP_remainingParents <- function(LHS_name, LHS_value, RHS_vals){
		curVarParents <- parents[[LHS_name]]
		expandVars <- curVarParents[!(curVarParents %in% names(RHS_vals))]
		expandedGrid <- expand.grid(posVals[expandVars])

		p <- 0
		for(egIndex in 1:nrow(expandedGrid)){
			# Applying the conditional law of total probability
			# P(X|Y) = Sum_{z}[P(X|Y,z)P(z|Y)]
			# Let P_1 = P(X|Y,z)
			#     P_2 = P(z|Y)
			
			# expandedGrid holds values of z
			z <- rowToList(expandedGrid[egIndex,],names(expandedGrid))
			x <- list()
			x[[LHS_name]] <- LHS_value
			y <- RHS_vals
			yz <- c(z,y)
			# print("---z")
			# print(z)
			# print("---y")
			# print(y)
			# print("---x")
			# print(x)
			# print("---yz")
			# print(yz)
			P_1 <- CP_full(x,yz)
			P_2 <- CP(z,y)
			p <- p + P_1*P_2
		}
		return(p)
	}

	CP_from_JP_ratio <- function(LHS_vals,RHS_vals){
		numer_vals <- c(LHS_vals,RHS_vals)
		#print(paste0(makeJPString(numer_vals),"/",makeJPString(RHS_vals)))
		numer <- JP(numer_vals)
		denom <- JP(RHS_vals)
		p <- numer/denom
		return(p)
	}
	#counterCP <- NA
	#maxCP <- 5
	CPs <- list(
		JD = function(LHS_vals,RHS_vals){
			if(length(RHS_vals)){
				p_numer <- JPs$JD(c(LHS_vals,RHS_vals))
				p_denom <- JPs$JD(RHS_vals)
				p <- p_numer/p_denom
			}else{
				JPs$JD(LHS_vals)
			}
			return(p)
		},
		CD = function(LHS_vals,RHS_vals){
			#counterCP <<- counterCP + 1
			#print(paste0("Calculating ",makeCPString(LHS_vals,RHS_vals)))
			# 1) If RHS_vals is empty
			#	calculate JP on LHS values
			# 2) If the form is P(X|Y) and X is one variable:
			# 	2.1) If the CP is of the form P(X|Pa(X))
			#		simple table lookup
			#	2.2) If all of Y is in pa(X) 
			#		use the conditional law of total probability 
			#		P(X|Y) = Sum_{z}[P(X|Y,z)P(z|Y)] 
			#		over the other parents Z 
			#		where pa(X) = {Y,Z}, intersection(Y,Z)=empty
			# 	2.3) If some/none of Y is in pa(X)
			#		Calculate JP of numerator and denominator	
			# 3) If X is multivariate:
			#	Calculate JP of numerator and denominator

			if(length(LHS_vals) == 0){
				print("Error: CP attempted without LHS variables")
				q("no")
			}

			if(!length(RHS_vals)){
				#print("P(Y|empty set)")
				#if(!is.na(counterCP) && counterCP == maxCP){q("no")}
				#1) P(Y|empty set)
				p <- JP(LHS_vals)

			}else if(length(LHS_vals) == 1){
				#print("P(X|Y) X is one variable")
				#2) P(X|Y) X is one variable
				LHS_var <- names(LHS_vals)[[1]]
				LHS_value <- LHS_vals[[LHS_var]]
				par <- parents[[LHS_var]]
				form_pXgivenPa <- is_form_pXgivenPa(LHS_vals,RHS_vals)
				if(form_pXgivenPa){
					#print("CP is of the form P(X|Pa(X))")
					#if(!is.na(counterCP) && counterCP == maxCP){q("no")}
					#2.1) the CP is of the form P(X|Pa(X))
					p <- CP_full(LHS_vals,RHS_vals)

				}else if(
					prod(names(RHS_vals) %in% par)
				){
					#print("All of Y is in pa(X)")
					#if(!is.na(counterCP) && counterCP == maxCP){q("no")}
					#2.2) all of Y is in pa(X)
					p <- cond_LTP_remainingParents(LHS_var, LHS_value, RHS_vals)

				}else{
					#print("Some/none of Y is in pa(X)")
					#if(!is.na(counterCP) && counterCP == maxCP){q("no")}
					#2.3) some/none of Y is in pa(X)
					p <- CP_from_JP_ratio(LHS_vals, RHS_vals)
				}

			}else{
				#print("P(X|Y) X is multivariate")
				#if(!is.na(counterCP) && counterCP == maxCP){q("no")}
				#3)
				p <- CP_from_JP_ratio(LHS_vals, RHS_vals)
			}
			
			return(p)
		}
	)

	CP <- function(LHS_vals,RHS_vals){
		return(CPs[[probMode]](LHS_vals,RHS_vals))
	}

	features <- list()
	resetFeatures <- function(){
		for(featureName in names(featureInstructions)){
			features[[featureName]] <<- NA
		}
	}
	resetFeatures()

	measureATE_NoLoop <- function(){
		#print("Measuring ATE without loop")
		Y_Table <- pXgivenPa[[outcome]]
		W_string <- paste0(confounders,collapse=", ")
		
		A1_confs <- paste0(paste0("a1.",confounders, " as ",confounders),collapse=", ")
		joinString <- paste0(paste0("a1.",confounders, "=a0.",confounders),collapse=" and ")

		A1_query <- paste0("select ",W_string,", P from Y_Table where ",exposure," = 1 and ",outcome," = 1 order by ",W_string)
		A0_query <- paste0("select ",W_string,", P from Y_Table where ",exposure," = 0 and ",outcome," = 1 order by ",W_string)
		RD_query <- paste0(paste0("select ",A1_confs,", a1.P - a0.P as RD_W from (",A1_query,") as a1 join (",A0_query,") as a0 on ",joinString),collapse="")
		W_query <- paste0("select ",W_string,", Sum(P) as P from fullJD group by ",W_string," order by ",W_string)
		joinString2 <- paste0(paste0("j.",confounders, "=w.",confounders),collapse=" and ")
		
		query <- paste0(c("select sum(j.RD_W*w.P) as ATE from (",RD_query,") as j join (",W_query,") as w on ",joinString2),collapse="")
		ATE <- sqldf(query)[1,"ATE"]
		return(ATE)
	}

	measureATE_Loop <- function(){
		# Use for debugging
		# Should give same answer as NoLoop version, but this one is slower
		features$ATE <<- 0
		conf_expansion <- expand.grid(posVals[confounders])
		for(i in 1:nrow(conf_expansion)){
			#Sum_z[[P(Y|A=1,z)-P(Y|A=0,z)]P(z)]
			z <- as.list(conf_expansion[i,])
			expList <-list()
			outList <- list()
			outList[[outcome]] <- 1
			expList[[exposure]] <- 1
			pY_A1z <- CP(outList,c(expList,z))
			expList[[exposure]] <- 0
			pY_A0z <- CP(outList,c(expList,z))
			p_z <- JP(z)
			component <- (pY_A1z - pY_A0z)*p_z
			features$ATE <<- features$ATE + component
		}
	}

	featureHandlers <- list(
	)

	setupMethodHandlers <- function(){
		#Download method files from github
		githubPaths <- c()
		for(method in names(inputs$methods)){
			if("github" %in% names(inputs$methods[[method]])){
				githubPath <- inputs$methods[[method]]$github
				githubPaths <- c(githubPaths,githubPath)
			}
		}
		githubPaths <- unique(githubPaths)

		suppressWarnings(dir.create(file.path("Function_Downloads")))
		suppressWarnings(dir.create(file.path("Function_Downloads","Analysis")))
		for(githubPath in githubPaths){
				download.file(paste0("https://raw.githubusercontent.com/ScottZimmerman/StudySimulator/master/Analysis/",githubPath), file.path("Function_Downloads","Analysis",githubPath),method="wget")
		}

		for(method in names(inputs$methods)){
			if("github" %in% names(inputs$methods[[method]])){
				githubPath <- inputs$methods[[method]]$github
				methodHandlers[[inputs$methods[[method]]$handler]] <<- dget(file.path("Function_Downloads","Analysis",githubPath))(exposure,outcome)
			}
		}
	}

	setupFeatureHandlers <- function(){
		#Download feature files from github
		githubPaths <- c()
		for(feature in names(inputs$features)){
			if("github" %in% names(inputs$features[[feature]])){
				githubPath <- inputs$features[[feature]]$github
				githubPaths <- c(githubPaths,githubPath)
			}
		}
		githubPaths <- unique(githubPaths)

		suppressWarnings(dir.create(file.path("Function_Downloads")))
		suppressWarnings(dir.create(file.path("Function_Downloads","Features")))
		for(githubPath in githubPaths){
				download.file(paste0("https://raw.githubusercontent.com/ScottZimmerman/StudySimulator/master/Features/",githubPath), file.path("Function_Downloads","Features",githubPath),method="wget")
		}

		for(feature in names(inputs$features)){
			if("github" %in% names(inputs$features[[feature]])){
				githubPath <- inputs$features[[feature]]$github
				featureHandlers[[inputs$features[[feature]]$handler]] <<- dget(file.path("Function_Downloads","Features",githubPath))(exposure,outcome)
			}
		}
	}
	setupDesignHandlers <- function(){}
	setupModelHandlers <- function(){}

	setupHandlers <- function(){
		setupFeatureHandlers()
		setupDesignHandlers()
		setupModelHandlers()
		setupMethodHandlers()
	}
	setupFeatureHandlers()

	#Load additional feature functions
	for(featureData in featureInstructions){
		if("script" %in% names(featureData)){
			featureHandlers[[featureData$handler]] <- dget(featureData$script)()
		}
	}

	measureFeatures <- function(){
		print("MEASURING FEATURES")
		resetFeatures()
		print("RESET")
		print(features)
		#First handle the default features which have to be calculated in order:
		#OF->RD->CONF and ATE->Conf
		print("OF")
		print(names(featureHandlers))
		featureHandlers$OF("OF",list())
		print("AFTER")
		featureHandlers$RD("RD",list())
		featureHandlers$ATE("ATE",list())
		featureHandlers$Conf("Conf",list())
		
		#Now handle everything else
		for(featureName in names(featureInstructions)){
			if(!(featureName %in% c("OF","RD","ATE","Conf"))){
				handler <- featureInstructions[[featureName]]$handler
				options <- featureInstructions[[featureName]]$options
				featureHandlers[[handler]](featureName,options)
			}
		}
	}

	saveFeatures <- function(folderName){
		folderPath <-file.path("distributions",folderName)
		write.csv(as.data.frame(features),file.path(folderPath,paste0("features.csv")),row.names=FALSE)
	}

	saveParametricSCM <- function(folderName){
		print("Saving Parametric SCM")
		folderPath <-file.path("distributions",folderName)
		dir.create(folderPath)
		for(var in names(pXgivenPa)){
			write.csv(pXgivenPa[[var]],file.path(folderPath,paste0(var,".csv")),row.names=FALSE)
		}
	}

	fullJD <- NA
	makeJDTable <- function(){
		print("Making JD Table")
		#JD mode only
		if(probMode == "JD"){
			fullJD <<- expand.grid(c(posVals,list(P=NA)))
			print(paste0("N rows: ",nrow(fullJD)))
			for(i in 1:nrow(fullJD)){
				vals <- as.list(fullJD[i,fullTopoOrder])
				fullJD[i,"P"] <<- JPs$CD(vals)
			}
		}
	}

	makeJDTable_NoLoop <- function(){
		#print("Creating Joint Distribution")
		#Create the JD table using table joins

		#Initialize the JD to the first table
		JD <- pXgivenPa[[fullTopoOrder[[1]]]]

		handledVars <- c(fullTopoOrder[[1]])
		for(tableName in  fullTopoOrder[2:length(fullTopoOrder)]){
			#print(tableName)
			tempTable <- pXgivenPa[[tableName]]

			JD_vars <- handledVars
			tempTable_vars <- c(parents[[tableName]],tableName)
			JD_only_vars <- setdiff(JD_vars,tempTable_vars)
			tempTable_only_vars <- setdiff(tempTable_vars,JD_vars)
			common_vars <- intersect(JD_vars,tempTable_vars)
					
			#Select common and JD only from JD, and select tempTable only from tempTable
			selectArgs_JD <- paste0("j.",c(common_vars,JD_only_vars))
			selectArgs_tT <- paste0("t.",tempTable_only_vars)
			selectString <- paste0(c(selectArgs_JD,selectArgs_tT,"j.P*t.P as P"),collapse=", ")

			#Join on all the common variables
			if(length(common_vars)){		
				joinString <- paste0(paste0("j.",common_vars,"=t.",common_vars),collapse = " and ")
				query <- paste0(c("select ",selectString," from JD as j join tempTable as t on ", joinString),collapse="")
			}else{
				query <- paste0(c("select ",selectString," from JD as j join tempTable as t"),collapse="")
			}
			JD <- sqldf(query)

			handledVars <- c(handledVars,tableName)
		}
		fullJD <<- JD
	}

	saveJDTable <- function(folderName){
		folderPath <-file.path("distributions",folderName)
		write.csv(fullJD,file.path(folderPath,"fullJD.csv"),row.names=FALSE)
	}

	loadJDTable <- function(folderName){
		filePath <-file.path("distributions",folderName,"fullJD.csv")
		if(file.exists(filePath)){
			fullJD <<- read.csv(filePath)
		} 
	}

	loadParametricSCM <- function(folderName){
		for(var in names(pXgivenPa)){
			pXgivenPa[[var]] <<- read.csv(file.path("distributions", folderName,paste0(var,".csv")))
		}
		jdPath <- file.path("distributions", folderName,"fullJD.csv")
		if(!file.exists(jdPath)){
			makeJDTable()
		}
	}

	makeParametricSCM_noLoop <- function(folderName){
		#print("Creating Parametric SCM")
		resetDist()
		for(curVar in names(pXgivenPa)){
			if(nParams[[curVar]] > 0){
				nClasses <- length(posVals[[curVar]])
				tempTable <- pXgivenPa[[curVar]]
				origNames <- paste0(names(tempTable),collapse=", ")
				par <- parents[[curVar]]
				groupVars <- paste0(par,collapse=", ")

				if(length(par)){
					query <- paste0("select * from tempTable order by ",groupVars)
				}else{
					query <- paste0("select * from tempTable")
				}
				
				tempTable <- sqldf(query)
				
				# Create a random ordering of the ids, allocate part of the remaining distribution to each one in turn
				tempTable[,"Prand"] <- runif(nParams[[curVar]])

				#Apply random order to elements of each parent group
				ord <- c(replicate(nrow(tempTable)/nClasses,sample(1:nClasses)))
				tempTable[,"OrderInGroup"] <- ord 
				if(length(par)){
					query <- paste0("select * from tempTable order by ",groupVars,", OrderInGroup")
				}else{
					query <- paste0("select * from tempTable order by OrderInGroup")
				}
				tempTable <- sqldf(query)
				tempTable[,"OrderInGroup"] <- NULL
				
				remainingP <- rep(1,nrow(tempTable)/nClasses)
				
				#In this randomly-assigned order, assign the remaining part of the distribtuion
				indices <-seq(1,nrow(tempTable),nClasses)
				for(i in 1:(nClasses - 1)){
					tempTable[indices,"P"] <- tempTable[indices,"Prand"]*remainingP
					remainingP <- remainingP-tempTable[indices,"P"]
					indices <- indices + 1
				}
				tempTable[indices,"P"] <- remainingP
				
				#Return to original order
				tempTable <- sqldf(paste0("select * from tempTable order by ID"))
				pXgivenPa[[curVar]] <<- tempTable			
			}
		}
		jdPath <- file.path("distributions", folderName,"fullJD.csv")
		makeJDTable_NoLoop()
	}

	makeParametricSCM_FromTheta <- function(theta){
		resetDist()
		theta_index <- 1
		for(curVar in names(pXgivenPa)){
			if(nParams[[curVar]] > 0){
				nClasses <- length(posVals[[curVar]])
				tempTable <- pXgivenPa[[curVar]]
				origNames <- paste0(names(tempTable),collapse=", ")
				par <- parents[[curVar]]
				groupVars <- paste0(par,collapse=", ")

				if(length(par)){
					query <- paste0("select * from tempTable order by ",groupVars)
				}else{
					query <- paste0("select * from tempTable")
				}
				
				tempTable <- sqldf(query)
				
				tempTable[,"Prand"] <- theta[theta_index:(theta_index-1+nParams[[curVar]])]
				theta_index <- theta_index+nParams[[curVar]]

				remainingP <- rep(1,nrow(tempTable)/nClasses)
				
				#In this assign the remaining part of the distribution
				indices <-seq(1,nrow(tempTable),nClasses)
				for(i in 1:(nClasses - 1)){
					tempTable[indices,"P"] <- tempTable[indices,"Prand"]*remainingP
					remainingP <- remainingP-tempTable[indices,"P"]
					indices <- indices + 1
				}
				tempTable[indices,"P"] <- remainingP
				
				#Return to original order
				pXgivenPa[[curVar]] <<- tempTable			
			}
		}
		makeJDTable_NoLoop()
	}

	#----------------------------------------
	# FEATURE DISTRIBUTION
	#----------------------------------------
	featuresDist <- data.frame(matrix(nrow=0, ncol=(1+length(features)))) 
	names(featuresDist) <- c("seed",names(features))
	createFeatureDistribution <- function(){
		for(i in 1:nFeaturesReps){
			seed <- floor(1000000000*runif(1))
			print(i)
			makeParametricSCM_noLoop(seed)
			measureFeatures()
			row <- as.data.frame(c(list("seed"=seed),features))
			featuresDist[i,] <<- row[,names(featuresDist)]
		}
	}
	saveFeaturesDistribution <- function(folderPath,index,seed){
		write.csv(featuresDist[index,],file.path(folderPath,paste0("featuresDist_",seed,".csv")),row.names=FALSE)
	}

	#----------------------------------------
	# SIMULATION
	#----------------------------------------
	getCDF <- function(varName,parentNames,parentVals){
		tempTable <- pXgivenPa[[varName]]	
		query <- paste0("select ",varName,", P from tempTable")
		if(length(parentVals)){
			#Select conditionally on the parents' values
			#Need quotes around the factor variables
			quotes <- ifelse(as.logical(sapply(as.data.frame(tempTable[,parentNames]),is.factor)),"'","")
			items <- paste0(parentNames,"=",quotes,parentVals,quotes)
			query <- paste0(query," where ",paste0(items,collapse=" and "))
		}
		queryOrder <- fullTopoOrder[fullTopoOrder %in% c(parentNames,varName)]
		query <- paste0(query," order by ",queryOrder)
		res <- sqldf(query)
		res[,"P"] <- cumsum(res[,"P"])
		return(res)
	}

	#Sample observations from the bayesian network P(X|Pa(X))
	sampleFromJD <- function(n){
		indices <- sample(1:nrow(fullJD),n,replace=TRUE,prob=fullJD[["P"]])
		obs <- fullJD[indices,names(fullJD)[!(names(fullJD) %in% "P")]]
		return(obs)
	}
	sampleFromBN <- function(n){
		#Set up the observations data frame
		nVar <- length(fullTopoOrder)
		obs <- data.frame(matrix(nrow=n,ncol=nVar))
		names(obs) <- fullTopoOrder

		# We are using independent errors (Identity covariance matrix) corresponding to correlated errors/dashed arrows in a dag
		U <- mvrnorm(n,rep(0,nVar),diag(nVar))
		U <- as.data.frame(pnorm(U))
		names(U) <- fullTopoOrder
		
		#Observations are simulated by applying the inverse cumulative distribution function
		for(obsIndex in 1:n){
			print(paste0("Obs #",obsIndex))
			for(varName in fullTopoOrder){
				pa <- parents[[varName]]
				cdf <- getCDF(varName,pa,obs[obsIndex,pa])
				cdfIndex <- min(which(cdf[,"P"] >= U[obsIndex,varName]))
				if(is.factor(pXgivenPa[[varName]][[varName]])){
					obs[obsIndex,varName] <- as.character(cdf[cdfIndex,varName]) #TYPES ARE NOT ACTING SMOOTHLY...
				}else{
					obs[obsIndex,varName] <- cdf[cdfIndex,varName] #TYPES ARE NOT ACTING SMOOTHLY...
				}
			}
		}
		return(obs)
	}
	sampleFromBN_NoLoop <- function(n){
		#To do using joins
		#Need to have CDFs probs in pXgivenPa tables
		#Then pull the row for the min cdf that is greater than U for the matching parent values
	}

	makeObs <- function(n){
		if(probMode == "JD"){
			return(sampleFromJD(n))
		}else if(probMode == "CD"){
			return(sampleFromBN(n))
		}else{
			print("Error: probMode incorrect in makeObs")
			q("no")
		}
	}

	#----------------------------------------
	# ANALYSIS METHOD PERFORMANCE CALCULATION
	#----------------------------------------
	methodHandlers <- list(
		unadjusted = function(an_data,options){
			EY1 <- mean(an_data[[outcome]][an_data[[exposure]]==1])
		    EY0 <- mean(an_data[[outcome]][an_data[[exposure]]==0])
		    return(EY1-EY0)
		},
		IPTW = function(an_data,options){
			g_model <- models$g[[options$g]]
			g <- predict(g_model, newdata = an_data, type = 'response')
			weight1 <- ifelse(an_data[[exposure]] == 1, (1/g), 0)
	        weight0 <- ifelse(an_data[[exposure]] == 0, (1/(1-g)), 0)  
	        EY0 <- mean(weight0 * an_data[[outcome]])
	        EY1 <- mean(weight1 * an_data[[outcome]])
	        return(EY1-EY0)
		},
		gComp = function(an_data,options){
			Q_model <- models$Q[[options$Q]]
			tx <- ct <- an_data
			tx[[exposure]] <- 1
			ct[[exposure]] <- 0

			data_w_cfEsts <- rbind(an_data, tx, ct)
			q <- predict(Q_model, newdata = data_w_cfEsts, type = 'response')
		    n <- length(q)/3

		    EY1 <- mean(q[(n+1):(2*n)]) 
		    EY0 <- mean(q[(2*n+1):(3*n)])
		    return(EY1-EY0)
		},
		TMLE = function(an_data,options){
			g_model_RHS <- modelForms$g[[options$g]]
			Q_model_RHS <- modelForms$Q[[options$Q]]
		    Qform <- c()
		    Qform[outcome] <- paste0("Q.kplus1~",Q_model_RHS)
			ltmle.out <- ltmle(
		      an_data,
		      Anodes=exposure,
		      Ynodes=outcome,
		      Qform = Qform,
		      gform = paste0(exposure,"~",g_model_RHS),
		      abar=c(1),
		      estimate.time = FALSE
		    )
		    EY1 <- ltmle.out$estimates[["tmle"]]
		    
		    ltmle.out <- ltmle(
		      an_data,
		      Anodes=exposure,
		      Ynodes=outcome,
		      Qform = Qform,
		      gform = paste0(exposure,"~",g_model_RHS),
		      abar=c(0),
		      estimate.time = FALSE
		    )
		    EY0 <- ltmle.out$estimates[["tmle"]]
		    return(EY1-EY0)
		}
	)

	calculateModels <- function(obs){
		mod.family <- binomial(link = as.character("logit"))

		# g models
		for(gModelName in names(modelForms$g)){
			models$g[[gModelName]] <<- glm(as.formula(paste0(exposure,"~",modelForms$g[[gModelName]])), family = mod.family, data = obs)
		}
		
		# Q models
		for(QModelName in names(modelForms$Q)){
			models$Q[[QModelName]] <<- glm(as.formula(paste0(outcome,"~",modelForms$Q[[QModelName]])), family = mod.family, data = obs)
		}
	}

	nAnalysisMethods <- length(analysisMethods)
	estimates <- NA
	CIs <- NA
	pValues <- NA
	resetEstimates <- function(){
		#Reset stored estimates, CIs and p-values
		estimates <<- data.frame(matrix(nrow=nEstimateReps,ncol=length(analysisMethods)))
		names(estimates) <<- names(analysisMethods)
		
		CIs <<- list(
			LB=data.frame(matrix(nrow=nEstimateReps,ncol=length(analysisMethods))),
			UB=data.frame(matrix(nrow=nEstimateReps,ncol=length(analysisMethods)))
		)
		names(CIs$LB) <<- names(analysisMethods)
		names(CIs$UB) <<- names(analysisMethods)
		
		pValues <<- data.frame(matrix(nrow=nEstimateReps,ncol=length(analysisMethods)))
		names(pValues) <<- names(analysisMethods)
	}
	resetEstimates()

	conductAnalyses <- function(repIndex,sampleSize){
		#set.seed(repIndex)
		obs <- makeObs(sampleSize)
		calculateModels(obs)
		for(method in names(analysisMethods)){
			methodData <- analysisMethods[[method]]
			res <- methodHandlers[[methodData$handler]](obs,methodData$options)
			estimates[repIndex,method] <<- res
		}
	}

	#----------------------------------------
	# PERFORMANCE ANALYSIS
	#----------------------------------------
	performance <- data.frame(matrix(nrow=0,ncol=(5*length(analysisMethods)+2)))
	ams <- names(analysisMethods)
	names(performance) <- c("seed","ATE",paste0(ams,"_Avg"),paste0(ams,"_Bias"),paste0(ams,"_Var"),paste0(ams,"_PercBias"),paste0(ams,"_MSE"))

	runAnalysesManyTimes <- function(printInterval=100){
		resetEstimates()
		for(i in 1:nEstimateReps){

			#Only print the first node on parallel runs
			if(i%%printInterval == 0){	
				if(compute_resource=="desktop"){
					print(paste0("analysis iteration ",i))
				}else{
					if(node_ID=="0"){
						print(paste0("analysis iteration ",i))
					}
				}
			}
			conductAnalyses(i,sampleSize)
		}
	}

	calculatePerformance <- function(seed,featureRepIndex){
		avgs <- apply(estimates,2,mean)
		biases <- avgs - features$ATE
		vars <- apply(estimates,2,var)
		percentBiases <- (avgs-features$ATE)/features$ATE
		MSEs <- biases^2+vars

		performance[featureRepIndex,] <<- c(seed,features$ATE,avgs,biases,vars,percentBiases,MSEs)
	}

	savePerformance <- function(folderPath,index,seed){
		write.csv(performance[index,],file.path(folderPath,paste0("performance_",seed,".csv")),row.names=FALSE)
	}

	timeMarkers <- list(
		makeSCM_start = NA,
		measureFeatures_start = NA,
		calculateEstimates_start = NA,
		calculatePerformance_start = NA
	)

	calculateComputationSpeed <- function(){
		tSCM <- difftime(timeMarkers$measureFeatures_start,timeMarkers$makeSCM_start)
		tFeatures <- difftime(timeMarkers$calculateEstimates_start,timeMarkers$measureFeatures_start)
		tEstimates <- difftime(timeMarkers$calculatePerformance_start,timeMarkers$calculateEstimates_start)
		return(data.frame(tSCM,tFeatures,tEstimates))
	}

	uploadResults <- function(seed){
		print("MONGO WAS CALLED HERE")
		# if(mongo_config_path!="NONE" && mongo_config_path!="" && mongo_config_path!="0" && mongo_config_path!=0){
		# 	print(mongo_config_path)
		# 	system(paste0("python ",file.path("save_results_mongoDB.py")," ",mongo_config_path," ",input_folder," ", input_json," ",output_folder," ",seed))
		# }
	}

	checkFilterMatch <- function(){
		doAnalyses <- 1
		dTotal <- 0
		for(feature in names(inputs$featureTargets)){
			d <- (features[[feature]]-inputs$featureTargets[[feature]])/inputs$featureTargets[[feature]]
			dTotal <- dTotal + d^2
			doAnalyses <- doAnalyses*(abs(d)<0.5)
		}
		dTotal <- sqrt(dTotal)
		print(dTotal)
		print(doAnalyses)
		return(doAnalyses)
	}

	minD <- 99999
	find_CS_NSGA2 <- function(targets,
			NSGA2_Options = list(
				generations = 16,
				popsize = 4 #popsize must be a multiple of 4
			)
		){

		minD <<- 99999
		
		objectiveFn <- function(theta){
			makeParametricSCM_FromTheta(theta)
			measureFeatures()

			#Compare features to targets
			varOrder <- names(targets)
			# d <- sum((as.data.frame(features[names(targets)])[,varOrder]-as.data.frame(targets)[,varOrder])^2)
			scaledValues <- ((as.data.frame(features[names(targets)])[,varOrder]-as.data.frame(targets)[,varOrder])/as.data.frame(targets)[,varOrder])^2
			print(scaledValues)
			if(length(scaledValues) > 1){
				d <- sum(scaledValues) + var(scaledValues[1,])
			}else{
				d <- sum(scaledValues)
			}
			
			if(d < minD){
				print(cbind(d,as.data.frame(features[names(targets)])))
			}
			minD <<- min(d,minD)

			return(d)
		}

		nsga2_results <- nsga2(
			popsize = NSGA2_Options$popsize,
			generations = NSGA2_Options$generations,
			objectiveFn, 
			idim = nNonFixedParams, 
			odim = 1, 
			lower.bounds = rep(0.01,nNonFixedParams), 
			upper.bounds = rep(0.99,nNonFixedParams)
		)
		return(nsga2_results)
	}

	createPerformanceAndFeaturesDistributions <- function(saveFolder,saveInterval=1,filter=0,searchMode="random"){
		savePath <-file.path(saveFolder,"raw")
		if(!file.exists(savePath)){
			dir.create(savePath,recursive=TRUE)
		}

		if(filter){
			setupFeatureTargets(saveFolder)
		}

		featureRepIndex <- 0
		while(!finished){
			seed <- floor(1000000000*runif(1))
			set.seed(seed)
			featureRepIndex <- featureRepIndex + 1

			if(compute_resource=="desktop"){
				print(paste0("---------------------",featureRepIndex,"---------------------"))
			}else{
				if(node_ID=="0"){
					print(paste0("---------------------",featureRepIndex,"---------------------"))
				}
			}
			
			if(filter){
				#Feature information
				if(searchMode=="random"){
					makeParametricSCM_noLoop(seed)
					measureFeatures()
					doAnalyses <- checkFilterMatch()
				}else{
					currentFeatureTargets <- getFeatureTargets()
					CSs <- find_CS_NSGA2(currentFeatureTargets)
					makeParametricSCM_FromTheta(CSs$par[1,])
					measureFeatures()
					doAnalyses <- 1
				}
			}else{
				#Feature information
				timeMarkers$makeSCM_start <<- Sys.time()
				makeParametricSCM_noLoop(seed)
				
				timeMarkers$measureFeatures_start <<- Sys.time()
				measureFeatures()

				doAnalyses <- 1	
			}

			if(doAnalyses){
				featuresDistRow <- as.data.frame(c(list("seed"=seed),features))
				featuresDist[featureRepIndex,] <<- featuresDistRow[,names(featuresDist)]

				#Analysis information
				timeMarkers$calculateEstimates_start <<- Sys.time()
				runAnalysesManyTimes()
				timeMarkers$calculatePerformance_start <<- Sys.time()
				calculatePerformance(seed,featureRepIndex)
				
				timeMarkers$calculatePerformance_end <<- Sys.time()
				if(featureRepIndex%%saveInterval == 0){
					if(filter){
						filterSavePath <- file.path(saveFolder,"consistency",featureTargetControl$currentRow)
						saveFeaturesDistribution(filterSavePath,featureRepIndex,seed)
						savePerformance(filterSavePath,featureRepIndex,seed)
					}else{
						saveFeaturesDistribution(savePath,featureRepIndex,seed)
						savePerformance(savePath,featureRepIndex,seed)
					}
					print("uploading")
					uploadResults(seed)
				}
				if(compute_resource=="desktop"){
						print(calculateComputationSpeed())
				}else{
					if(node_ID=="0"){
						print(calculateComputationSpeed())
					}
				}
				
				if(!filter){
					if(node_ID=="0"){
						print("NODE ID 0")
						results <- list.files(file.path(saveFolder,"raw"))
						nResults <- length(results)/2
						
						print(paste0("nResults: ",nResults))
						if(nResults > nFeaturesReps){
							print("Compiling feature importance results")
							system(paste0(c("Rscript",file.path("Compile_Results.R"),saveFolder),collapse=" "))
							print("Analyzing feature importance results")
							system(paste0(c("Rscript",file.path("Analyze_Results.R"),input_folder,input_json,output_folder),collapse=" "))

							print("Uploading feature importance results")
							system(paste0(c("python",file.path("save_featureImportance_mongoDB.py"),mongo_config_path,input_folder,input_json,output_folder),collapse=" "))
							system(paste0(c("python",file.path("change_job_status.py"), mongo_config_path, job$user, job$id_by_user,"finished","1"),collapse=" "))
							system(paste0("scancel ",Sys.getenv("SLURM_JOB_ID")))
						}
					}
				}else{
					measure_consistency(saveFolder)
				}
			}
		}
	}

	getTheta <- function(){
		theta <- c()
		for(var in names(pXgivenPa)){
			theta <- c(theta,pXgivenPa[[var]]$P)
		}
		return(theta)
	}


	makeParametricDGS <- function(seed){
		makeParametricSCM_noLoop(seed)
	}

	getFeatures <- function(){
		return(features)
	}

	return(list(measureFeatures=measureFeatures,makeParametricDGS=makeParametricDGS,getFeatures=getFeatures))
}