# Calculate summary statistics for conditional densities/samples

# 1. Calculate the quantiles for a selected variable (same as in conditional_densities)
# 2. Calculate a number of summary statistics for all other variables conditioned on the 
# selected variable and print them in a list with an entry for each summary statistic.

#' Summary Statistics
#' 
#' Calculates the means, variances, skewness and kurtosis for a range of quantiles
#' of continuous variables. 
#'
#' @param var_name A variable from your \code{dataset}.
#' @param dataset A dataframe. 
#' @param n_quantiles Number of quantiles you want to partition \code{var_name} into.
#' @param n_sum_stats Number of summary statistics you want to print. The order of the 
#' statistics is as follows: conditional mean, conditional variance, conditional skewness, 
#' conditional kurtosis. 
#'
#' @return A list with one entry for each summary statistic.
#' @export
#'
#' @examples 
#' n <- 500
#' number_variables <- 5
#' n_quantiles <- 4
#' 
#' set.seed(42)
#' variables <- as.data.frame(sapply(1:number_variables, function(a){
#'   if (runif(1) < 0.5) rnorm(n)
#'     else rgamma(n, 0.5)
#'}))
#'  
#' sum_stats("V1", variables, n_quantiles, n_sum_stats = 4)  
sum_stats <- function(var_name, dataset, n_quantiles = 5, n_sum_stats = 3){
  
  #1.1 categorical var_name
  if(is.factor(dataset[,var_name])){
    if(n_quantiles != length(levels(dataset[,var_name]))){
      warning(paste0(var_name," is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quant <- as.factor(as.numeric(dataset[,var_name]))
    
  }
  
  #1.2 continuous var_name
  else {
    if(n_quantiles == 1){ #only one quantile
      data_help <- dataset
      data_help$quant <- 1
      data_help$quant <- as.factor(data_help$quant)
    } else {
      var_goal <- dplyr::select(dataset, var_name)[,1]
      quantiles <- stats::quantile(var_goal, 1:(n_quantiles-1)/(n_quantiles))
      quantiles <- as.numeric(quantiles)
      data_help <- dataset
      data_help$quant <- 1 + findInterval(var_goal, quantiles)
      data_help$quant <- as.factor(data_help$quant)
      names_var <- names(dataset)
    }
  }
  
  
  #2. summary statistics
  
  # Create placeholders for output list 
  names_var <- names(dataset)
  stats_names <- c("conditional_mean", "conditional_variance", "conditional_skewness",
                   "conditional_kurtosis")
  sum_stats <- vector("list", n_sum_stats) 
  names(sum_stats) <- stats_names[1:length(sum_stats)]
  
  for(i in 1:length(sum_stats)){
    sum_stats[[i]] <- matrix(0, nrow = n_quantiles, ncol = ncol(dataset))
  }
  
  # name rows and cols of output
  for(i in 1:length(sum_stats)){
    rownames(sum_stats[[i]]) <- paste0("quantile", 1:n_quantiles)
    colnames(sum_stats[[i]]) <- names_var
  }
  
  # calculate conditional means 
  for(i in 1:n_quantiles){
    for (j in 1:ncol(dataset)) {
      
      #do not calculate means for non-continuous variables
      if(class(data_help[, j]) == "factor"){
        sum_stats$conditional_mean[i, j] <- NA
      } else {
        sub <- subset(data_help, quant == as.character(i), select = names_var[j])
        sum_stats$conditional_mean[i, j] <- round(mean(sub[,1]), 5)
      }
    }
  }
  
  # calculate conditional variances
  if(n_sum_stats > 1){
    for(i in 1:n_quantiles){
      for (j in 1:ncol(dataset)) {
        
        #do not calculate variance for non-continuous variables
        if(class(data_help[, j]) == "factor"){
          sum_stats$conditional_variance[i, j] <- NA
        } else {
          sub <- subset(data_help, quant == as.character(i), select = names_var[j])
          sum_stats$conditional_variance[i, j] <- round(stats::var(sub[,1]), 5)
        }
      }
    }
  }
  
  
  #calculate conditional skewness
  if(n_sum_stats > 2){
    for(i in 1:n_quantiles){
      for (j in 1:ncol(dataset)) {
        
        #do not calculate skewness for non-continuous variables
        if(class(data_help[, j]) == "factor"){
          sum_stats$conditional_skewness[i, j] <- NA
        } else {
          sub <- subset(data_help, quant == as.character(i), select = names_var[j])
          sum_stats$conditional_skewness[i, j] <- round(moments::skewness(sub[,1]),5)
        }
      }
    }
  }
  
  
  #calculate conditional kurtosis
  if(n_sum_stats > 3){
    for(i in 1:n_quantiles){
      for (j in 1:ncol(dataset)) {
        
        #do not calculate kurtosis for non-continuous variables
        if(class(data_help[, j]) == "factor"){
          sum_stats$conditional_kurtosis[i, j] <- NA
        } else {
          sub <- subset(data_help, quant == as.character(i), select = names_var[j])
          sum_stats$conditional_kurtosis[i, j] <- round(moments::kurtosis(sub[,1]),5)
        }
      }
    }
  }
 
  
  return(sum_stats)
  
}
  

