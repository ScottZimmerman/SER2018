function(){

	#W
	#A~W
	#Y~A+W+A*W

	#Continuous example
	functionOrder <- c("income","violence","depression")

	params_template <- list(
		income = c("scale","intercept"),
		violence = c("scale","intercept","income_coef"),
		depression = c("scale","intercept","income_coef","violence_coef","interaction_coef")
	)	

	structural_equation_templates <- list(
		income = function(params){

			f <- function(df,exog=NULL){

				if(is.null(exog)){
					exog <- rnorm(nrow(df))
				}

				x <- params$income$intercept 
				x <- x + params$income$scale*exog
				x[x<0] <- 0

				return(list(x=x,u=exog))
			}
			return(f)

		},
		violence = function(params){
			
			f <- function(df, exog=NULL){

				if(is.null(exog)){
					exog <- rnorm(nrow(df))
				}

				x <- params$violence$intercept 
				x <- x + params$violence$scale*exog
				x <- x + params$violence$income_coef*df[,"income"]
				x[x<0] <- 0
				
				return(list(x=x,u=exog))
			}
			return(f)

		},
		depression = function(params){
			
			f <- function(df, exog=NULL){
				if(is.null(exog)){
					exog <- rnorm(nrow(df))
				}

				x <- params$depression$intercept 
				x <- x + params$depression$scale*exog
				x <- x + params$depression$income_coef*df[,"income"]
				x <- x + params$depression$violence_coef*df[,"violence"]
				x <- x + params$depression$interaction_coef*df[,"income"]*df[,"violence"]
				x[x<0] <- 0

				return(list(x=x,u=exog))
			}
			return(f)

		}
	)
	
	set_parameters <- function(params){
		structural_equations <- list()
		for(f_template_name in names(structural_equation_templates)){
			structural_equations[[f_template_name]] <- structural_equation_templates[[f_template_name]](params)
		}
		return(structural_equations)
	}


	feature_functions <- list(
		unitIncreaseTreatmentEffect = function(structural_equations, df, params){
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
		},
		medianOutcome = function(structural_equations,df, params){
			feature <- median(df$depression)
			return(feature)
		},
		unitIncreaseTreatmentEffect = function(structural_equations, df, params){
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
		},
		interaction = function(structural_equations,df, params){
			#Or calculate the features directly from the parameters
			return(params$depression$interaction_coef)
		}
	)

	measure_features <- function(structural_equations, params,data_set_size=100000){
		#Test by running more than once, if results are substantially different, increase data_set_size
		very_large_data_set <- DGS_Test1$draw_observations(structural_equations,data_set_size)

		features <- list()
		df <- cbind(very_large_data_set$x,very_large_data_set$u)
		for(f in names(feature_functions)){
			features[[f]] <- feature_functions[[f]](structural_equations, df, params)
		}
		return(features)
	}

	draw_observations <- function(structural_equations,n){
		df <- data.frame(matrix(nrow=n,ncol=length(structural_equation_templates)*2))
		colnames(df) <- c(functionOrder,paste0("U_",functionOrder))

		for(f in names(structural_equations)){
			se_result <- structural_equations[[f]](df)
			df[,f] <- se_result$x
			df[,paste0("U_",f)] <- se_result$u				
		}

		data <- list(
			x=df[,functionOrder],
			u=df[,paste0("U_",functionOrder)]
		)
		return(data)
	}

	functions <- list(
		set_parameters=set_parameters,
		draw_observations=draw_observations,
		measure_features=measure_features
	)
	return(functions)
}