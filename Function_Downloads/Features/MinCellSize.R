function(outcome,exposure){
	f <- function(name,options){
		if(probMode == "JD"){
			query <- "select min(P) as min_P from fullJD"
			features[[name]] <<- sqldf(query)[,"min_P"]
		}else{
			features[[name]] <<- NA
		}
	}
	return(f)
}