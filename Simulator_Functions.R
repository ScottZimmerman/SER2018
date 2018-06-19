function(DGS, inputs){

	get_estimates <- function(obs_data,analysis_functions){
		estimates <- list()
		for(method in names(inputs$methods$methods)){
			method_instructions <- inputs$methods$methods[[method]]

			estimate <- analysis_functions[[method_instructions$handler]](obs_data,method_instructions$options)
			estimates[[method]] <- estimate
		}	
		return(estimates)
	}

	functions <- list(
		draw_parameters_uniform = function(paramRanges){
			params <- list()
			for(variable_name in names(paramRanges)){
				params[[variable_name]] <- list()
				for(parameter_name in names(paramRanges[[variable_name]])){
					LB <- paramRanges[[variable_name]][[parameter_name]]$min
					UB <- paramRanges[[variable_name]][[parameter_name]]$max
					params[[variable_name]][[parameter_name]] <- runif(1,min=LB,max=UB)
				}
			} 
			return(params)
		},
		measure_features = function(dgs, params, feature_instructions, data_set_size=100000){
			#Test by running more than once, if results are substantially different, increase data_set_size
			very_large_data_set <- DGS$draw_observations(dgs,data_set_size)

			features <- list()
			df <- cbind(very_large_data_set$x,very_large_data_set$u)

			for(f in names(feature_functions)){
				features[[f]] <- feature_functions[[f]](dgs, df, params)
			}			
			return(features)
		},
		get_inference = function(obs_data,R=1000){
			CIs <- data.frame(matrix(nrow=length(analysis_methods),ncol=2))
			colnames(CIs) <- c("LB","UB")
			rownames(CIs) <- names(analysis_methods)

			n <- nrow(obs_data)
			estimates_by_method <- data.frame(matrix(nrow=R,ncol=length(analysis_methods)))
			colnames(estimates_by_method) <- names(analysis_methods)

			for(i in 1:R){
				seed <- round(10000000*runif(1))
				set.seed(seed)
				indices <- sample(c(1:n),n,replace = T)
				estimates <- get_estimates(obs_data[indices,],analysis_methods)
				estimates_by_method[i,] <- estimates
			}
			for(method in names(analysis_methods)){
				CIs[method,] <- quantile(estimates_by_method[,method],c(0.05,0.95))
			}
			return(CIs)	
		},		
		calculate_estimate_distributions = function(dgs, analysis_methods, n_observations, n_estimates,R=1000){

			estimate_distributions <- data.frame(matrix(nrow=n_estimates,ncol=length(inputs$methods$methods)))
			colnames(estimate_distributions) <- names(inputs$methods$methods)
			CI_distributions <- list()
			for(method in names(analysis_methods)){
				CI_distributions[[method]] <- data.frame(matrix(nrow=n_estimates,ncol=2))
				colnames(CI_distributions[[method]]) <- c("LB","UB")
			}

			for(i in 1:n_estimates){
				cat(".")
				if(i%%100==0){
					cat(paste0(i,"\n"))
				}
				seed <- round(10000000*runif(1))
				set.seed(seed)
				data_xu <- DGS$draw_observations(dgs,n_observations)
				estimates_by_method <- get_estimates(data_xu$x,analysis_methods)
				for(method in names(estimates_by_method)){
					estimate_distributions[i,method] <- estimates_by_method[[method]]
				}

				if(R>0){
					CIs <- get_inference(data_xu$x,R=R)
					for(method in names(analysis_methods)){
						CI_distributions[[method]][i,] <- CIs[method,]
					}
				}
			}
			return(list(estimates=estimate_distributions,CIs=CI_distributions,seed=seed))
		},
		calculate_performance = function(estimate_distributions, truth, meaningful_error){
			estimates <- estimate_distributions$estimates

			performance_metric_names <- c("Mean","Bias","Percent_Bias","Variance","MSE","CIC","P(absError>meaningfulValue)")
			performance <- data.frame(matrix(nrow=ncol(estimates),ncol=length(performance_metric_names)))
			colnames(performance) <- performance_metric_names
			rownames(performance) <- colnames(estimates)

			errors <- estimates-truth
			for(method in colnames(estimates)){
				#Mean 
				performance[method,"Mean"] <- mean(estimates[,method])
				#Bias
				performance[method,"Bias"] <- performance[method,"Mean"]-truth
				
				#Percent_Bias
				performance[method,"Percent_Bias"] <- performance[method,"Bias"]/truth

				#Variance
				performance[method,"Variance"] <- var(estimates[,method])

				#MSE
				performance[method,"MSE"] <- performance[method,"Bias"]*performance[method,"Bias"] + performance[method,"Variance"]

				#CIC
				print("CIC disabled for now")
				if(FALSE){
					performance[method,"CIC"] <- sum((truth>=estimate_distributions$CIs[[method]]$LB)*(truth<=estimate_distributions$CIs[[method]]$UB))/nrow(estimate_distributions$CIs[[method]])
				}

				#P(error > meaningful value)
				performance[method,"P(absError>meaningfulValue)"] <- sum(abs(errors[,method])>meaningful_error)/nrow(estimates)

			}

			return(performance)
		}
	)
	return(functions)
}