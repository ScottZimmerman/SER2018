function(structural_equations, df, params){
	#One type of feature is measured using the counterfactuals for the units in the observed data set 
	#Create a new data set where you set the exposureto one unit higher than was observed
	do_mx <- df
	do_mx[,"violence"] <- df[,"violence"]+1

	exog <- df[,"U_depression"]

	depression_if_1unit_increase <- structural_equations$depression(do_mx,exog=exog)
	depression <- structural_equations$depression(df,exog=df[,"U_depression"])
	
	dif <- depression_if_1unit_increase$x - depression$x
	feature <- mean(dif)
	return(feature)
}
