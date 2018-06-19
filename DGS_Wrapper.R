function(DGS_object){
	data <- list()

	attachParameterRanges <- function(){}
	attachFeatureFunctions <- function(){}

	set_parameters <- function(){}
	draw_observations <- function(){}
	measure_features <- function(){}

	setup <- function(){
		required_functions <- c("set_parameters","draw_observations","measure_features")
		DGS_obj_functions <- names(DGS_object)
		missing_functions <- setdiff(required_functions,DGS_obj_functions)
		if(length(missing_functions)){
			print("DGS_object used to initialize DGS is missing functions:")
			for(mf in missing_functions){
				print(paste0("Missing function: ",mf))
			}
			q("no")
		}
	}

	
	functions <- list(
		set_parameters=set_parameters,
		draw_observations=draw_observations,
		measure_features=measure_features
	)
	functions <- list()
	return(functions)
}