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


	feature_functions <- list()
	bind_features <- function(loaded_feature_functions){
		print("binding")
		feature_functions <<- loaded_feature_functions
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
		bind_features=bind_features
	)
	return(functions)
}