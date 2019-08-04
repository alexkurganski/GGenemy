#Plot Conditional densities
#1.Calculate the quantiles for selected variable
#2.Plot densities of all other variables based on the calculated quantiles
plot_conditional_densities <- function(var_name, dataset, n_quantiles = 5){

  #1.1 categorial var_name
  if(is.factor(dataset[,var_name])){
    if(n_quantiles != length(levels(dataset[,var_name]))){
      warning(paste0(var_name," is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quantile <- levels(dataset[,var_name])

  }

  #1.2 continuous var_name
  else{
    var_goal <- select(dataset, var_name)[,1]
    quantiles <- quantile(var_goal, 1:(n_quantiles-1)/(n_quantiles))
    quantiles <- as.numeric(quantiles)
    data_help <- dataset
    data_help$quantile <- 1 + findInterval(var_goal, quantiles)
    data_help$quantile <- as.factor(data_help$quantile)
    names_var <- names(dataset)
  }

  #2.
  names_var <- names(dataset)
  g <- ggplot(data_help, aes(fill = quantile)) + theme_minimal()
  lapply(names_var, function(a){
    if(is.factor(data_help[,a]) & is.factor(data_help[,var_name])){
      g + geom_bar(aes_string(x = a,fill = var_name)) +
        ggtitle(paste0(a, " conditional on ", var_name))
    }
    else if(is.factor(data_help[,a])){
      g + geom_boxplot(aes_string(x = a,y = var_name)) +
        ggtitle(paste0(a, " conditional on ", var_name))
    }else{
    g + geom_density(alpha = 0.5) + aes_string(x = a) +
      ggtitle(paste0(a, " conditional on ", var_name))
  }
  })
}
