#Plot Conditional densities

#1.Calculate the quantiles for selected variable
#2.Plot densities of all other variables based on the calculated quantiles

#' Plot conditional densities
#' 
#' Plots densities of continuous variables, conditioned on quantiles of 
#' continuous variables or categories of categorical variables. 
#' For categorical variables, boxplots or barplots will be created, depending on whether
#' the variable to be conditioned on is continuous or categorical.
#'   
#'
#' @param var_name A variable from your \code{dataset}.
#' @param dataset A dataframe. 
#' @param n_quantiles Number of quantiles you want to partition \code{var_name} into.
#'
#' @return Several ggplot graphics. The total number of plots will equal the number of
#' variables in your \code{dataset}.
#' @export
#'
#' @examples
#' n <- 500
#' number_variables <- 5
#' 
#' set.seed(42)
#' variables <- as.data.frame(sapply(1:number_variables, function(a){
#'   if (runif(1) < 0.5) rnorm(n)
#'     else rgamma(n, 0.5)
#'}))
#'
#'plot_conditional_densities("V1", variables)     
plot_conditional_densities <- function(var_name, dataset, n_quantiles = 5){

  #1.1 categorical var_name
  if(is.factor(dataset[,var_name])){
    if(n_quantiles != length(levels(dataset[,var_name]))){
      warning(paste0(var_name," is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quant <- as.factor(as.numeric(dataset[,var_name]))

  }

  #1.2 continuous var_name
  else{
    var_goal <- dplyr::select(dataset, var_name)[,1]
    quantiles <- stats::quantile(var_goal, 1:(n_quantiles-1)/(n_quantiles))
    quantiles <- as.numeric(quantiles)
    data_help <- dataset
    data_help$quant <- 1 + findInterval(var_goal, quantiles)
    data_help$quant <- as.factor(data_help$quant)
    names_var <- names(dataset)
  }

  #2.
  names_var <- names(dataset)
  g <- ggplot2::ggplot(data_help, ggplot2::aes(fill = quant)) + ggplot2::theme_minimal()
  lapply(names_var, function(a){
    if(is.factor(data_help[,a]) & is.factor(data_help[,var_name])){
      g + ggplot2::geom_bar(ggplot2::aes_string(x = a,fill = var_name)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", var_name))
    }else if(is.factor(data_help[,a])){
      g + ggplot2::geom_boxplot(ggplot2::aes_string(x = a,y = var_name)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", var_name))
    }else{
    g + ggplot2::geom_density(alpha = 0.5) + ggplot2::aes_string(x = a) +
      ggplot2::ggtitle(paste0(a, " conditional on ", var_name))
  }
  })
}
