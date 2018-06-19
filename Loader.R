function(){
	functions <- list(	
		load_feature_functions = function(inputs){
			DGS_root <- file.path("DGSs",inputs$dgs$name)
			feature_functions <- list()
			for(handlerName in names(inputs$features$handlers)){
				handlerData <- inputs$features$handlers[[handlerName]]

				if(handlerData$source == "local"){
					handlerPath <- file.path(DGS_root,"feature_functions",handlerData$path)
					feature_function <- dget(handlerPath)
					feature_functions[[handlerName]] <- feature_function
				}else{
					print("Error: Can only handle local feature functions")
					q("no")
				}			
			}
			return(feature_functions)
		},
		load_analysis_functions = function(inputs){
			DGS_root <- file.path("DGSs",inputs$dgs$name)
			analysis_functions <- list()
			for(handlerName in names(inputs$methods$handlers)){
				handlerData <- inputs$methods$handlers[[handlerName]]

				if(handlerData$source == "local"){
					handlerPath <- file.path(DGS_root,"analysis_functions",handlerData$path)
					analysis_function <- dget(handlerPath)()
					analysis_functions[[handlerName]] <- analysis_function
				}else{
					print("Error: Can only handle local analysis functions")
					q("no")
				}			
			}
			return(analysis_functions)
		}
	)
	return(functions)
}