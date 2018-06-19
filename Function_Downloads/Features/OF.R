function(outcome,exposure){
	f <- function(name,options){
		if("OF" %in% names(features) && !is.na(features$OF)){
			features[[name]] <<- features$OF
		}else{
			outList <- list()
			expList <- list()
			outList[[outcome]] <- 1
			expList[[exposure]] <- 0
			features[[name]] <<- CP(outList, expList)
		}
	}
	return(f)
}
