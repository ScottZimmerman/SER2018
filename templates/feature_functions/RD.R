function(outcome,exposure){
	f <- function(name,options){
		if("RD" %in% names(features) && !is.na(features$RD)){
			features[[name]] <<- features$RD
		}else{
			outList <- list()
			expList <- list()
			outList[[outcome]] <- 1
			expList[[exposure]] <- 1
			PS1_N1 <- CP(outList,expList)
			features[[name]] <<- PS1_N1 - features$OF
		}
	}
	return(f)
}
