function(outcome,exposure){
	f <- function(name,options){
		tempTable <- pXgivenPa[[exposure]]
		#print("Calculating minimum treatment propensity")
		query <- "select min(P) as min_P from tempTable"
		features[[name]] <<- sqldf(query)[,"min_P"]
	}
	return(f)
}
