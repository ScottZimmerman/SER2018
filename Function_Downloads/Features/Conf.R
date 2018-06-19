function(outcome,exposure){
	f <- function(name,options){
		if("Conf" %in% names(features) && !is.na(features$Conf)){
			features[[name]] <<- features$Conf
		}else{
			features[[name]] <<- features$RD - features$ATE
		}
	}
	return(f)
}
