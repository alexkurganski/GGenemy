#' Plot multiple selected quantile densities 
#'
#' Plots densities of chosen continuous variables, conditioned on self selected quantiles of
#' continuous variables. For categorical
#' variables, boxplots will be created.
#'
#'
#' @param dataset A dataframe.
#' @param given_var A variable from your \code{dataset}.
#' @param firstquant Vector of two Values, with min and max value of the first self selected quantile
#' @param secondquant Vector of two Values, with min and max value of the second self selected quantile
#' @param thirdquant Vector of two Values, with min and max value of the third self selected quantile
#' @param var_to_plot A Vector of Variables to condition on from your \code{dataset}.
#'
#' @return Several ggplot graphics. The total number of plots will equal the 
#' number of chosen variables of your \code{dataset}.
#' @export
#'
#' @examples
#' n <- 500
#' number_variables <- 5
#'
#' set.seed(42)
#' variables <- as.data.frame(sapply(1:number_variables, function(a) {
#'   if (runif(1) < 0.5) {
#'     rnorm(n)
#'   } else {
#'     rgamma(n, 0.5)
#'   }
#' }))
#'
#' plot_multiple_selected_quantile_densities(variables, "V1", c(0,1),c(2,3), var_to_plot = c("V2","V3"))

# Plot single selected quantile densitiy

# 1.Choose the quantile for selected variable 
# 2.Plot the density of a chosen other variable based on the self selected quantiles
plot_multiple_selected_quantile_densities <- function(dataset, given_var, firstquant = NA, secondquant = NA, thirdquant = NA, var_to_plot) {
  
  # 1.1 categorial given_var
  if (is.factor(dataset[, given_var])) {
    if (n_quantiles != length(levels(dataset[, given_var]))) {
      warning(paste0(given_var, " is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quant <- as.factor(as.numeric(dataset[, given_var]))
  } else { # 1.2 continuous given_var
    data_help <- dataset
    data_help$quant <- "remaining"
    if(!is.na(any(firstquant))){
      firstq <- which(dataset[,given_var] >= firstquant[1] & dataset[,given_var] <= firstquant[2])
      data_help$quant[firstq] <- paste(firstquant[1], "to", firstquant[2])
    }
    if(!is.na(any(secondquant))){
      secondq <- which(dataset[,given_var] >= secondquant[1] & dataset[,given_var] <= secondquant[2])
      data_help$quant[secondq] <- paste(secondquant[1], "to", secondquant[2])
    }
    if(!is.na(any(thirdquant))){
      thirdq <- which(dataset[,given_var] >= thirdquant[1] & dataset[,given_var] <= thirdquant[2])
      data_help$quant[thirdq] <- paste(thirdquant[1], "to", thirdquant[2])
    }
    data_help$quant <- as.factor(data_help$quant)
  }
  
  #2. 
  names_var <- names(dataset)
  g <- ggplot2::ggplot(data_help, ggplot2::aes(fill = quant)) + ggplot2::theme_minimal()
  lapply(var_to_plot, function(a) {
    if (is.factor(data_help[, a]) & is.factor(data_help[, given_var])) {
      g + ggplot2::geom_bar(ggplot2::aes_string(x = a, fill = given_var)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else if (is.factor(data_help[, a])) {
      g + ggplot2::geom_boxplot(ggplot2::aes_string(x = a, y = given_var)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else {
      g + ggplot2::geom_density(alpha = 0.5) + ggplot2::aes_string(x = a) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    }
  })
}
