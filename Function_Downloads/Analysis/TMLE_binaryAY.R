function(exposure, outcome){
	f <- function(an_data,options){
		g_model_RHS <- modelForms$g[[options$g]]
		Q_model_RHS <- modelForms$Q[[options$Q]]
	    Qform <- c()
	    Qform[outcome] <- paste0("Q.kplus1~",Q_model_RHS)
		ltmle.out <- ltmle(
	      an_data,
	      Anodes=exposure,
	      Ynodes=outcome,
	      Qform = Qform,
	      gform = paste0(exposure,"~",g_model_RHS),
	      abar=c(1),
	      estimate.time = FALSE
	    )
	    EY1 <- ltmle.out$estimates[["tmle"]]
	    
	    ltmle.out <- ltmle(
	      an_data,
	      Anodes=exposure,
	      Ynodes=outcome,
	      Qform = Qform,
	      gform = paste0(exposure,"~",g_model_RHS),
	      abar=c(0),
	      estimate.time = FALSE
	    )
	    EY0 <- ltmle.out$estimates[["tmle"]]
	    return(EY1-EY0)
	}
	return(f)
}