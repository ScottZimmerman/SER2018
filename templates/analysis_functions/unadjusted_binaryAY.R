function(exposure, outcome){
	f <- function(an_data,options){
		EY1 <- mean(an_data[[outcome]][an_data[[exposure]]==1])
	    EY0 <- mean(an_data[[outcome]][an_data[[exposure]]==0])
	    return(EY1-EY0)
	}
	return(f)
}