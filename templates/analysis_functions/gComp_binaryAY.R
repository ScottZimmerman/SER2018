function(exposure, outcome){
	f <- function(an_data,options){
		Q_model <- models$Q[[options$Q]]
		tx <- ct <- an_data
		tx[[exposure]] <- 1
		ct[[exposure]] <- 0

		data_w_cfEsts <- rbind(an_data, tx, ct)
		q <- predict(Q_model, newdata = data_w_cfEsts, type = 'response')
	    n <- length(q)/3

	    EY1 <- mean(q[(n+1):(2*n)]) 
	    EY0 <- mean(q[(2*n+1):(3*n)])
	    return(EY1-EY0)
	}
	return(f)
}