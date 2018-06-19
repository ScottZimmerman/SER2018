function(exposure, outcome){
	#IPTW for binary A and Y
	f <- function(an_data,options){
		g_model <- models$g[[options$g]]
		g <- predict(g_model, newdata = an_data, type = 'response')
		weight1 <- ifelse(an_data[[exposure]] == 1, (1/g), 0)
	    weight0 <- ifelse(an_data[[exposure]] == 0, (1/(1-g)), 0)  
	    EY0 <- mean(weight0 * an_data[[outcome]])
	    EY1 <- mean(weight1 * an_data[[outcome]])
	    return(EY1-EY0)
	}
	return(f)
}