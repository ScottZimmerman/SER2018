function(outcome,exposure){
	f <- function(name,options){
		if("ATE" %in% names(features) && !is.na(features$ATE)){
			features[[name]] <<- features$ATE
		}else{
			features[[name]] <<- measureATE_NoLoop()
		}
	}
	return(f)
}
