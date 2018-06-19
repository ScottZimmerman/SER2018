function(){
	predict_effect <- function(model,obs_data,method_options){
		Ypred_given_Aobs <- predict(model,newdata=obs_data,type='response')
		
		cf_data <- obs_data
		cf_data[,method_options$exposure] <- cf_data[,method_options$exposure] + 1

		Ypred_given_AoneUp <- predict(model,newdata=cf_data,type='response')
		
		unitTreatmentEffect_est <- mean(Ypred_given_AoneUp)-mean(Ypred_given_Aobs)
		return(unitTreatmentEffect_est)
	}

	f <- function(obs_data,options){
		model <- glm(formula=as.formula(options$model),data=obs_data)
		effect <- predict_effect(model,obs_data,method_options=options)
		return(effect)
	}
	return(f)
}