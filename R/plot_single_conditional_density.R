#' Plot a single conditional density
#'
#' Plots the conditional density of a continuous variable conditioned on quantiles of
#' another continuous variable or categories of a categorical variable. Other plot types for other classes of variables.
#' 
#' If the variable of interest is continuous, the function will create 
#' a conditional density plot. Depending on whether the variable to be conditioned
#' on is continuous itself or a factor, the dependent variable will be
#' conditioned on its quantiles or categories.
#' If the variable of interest is categorical, a boxplot or barplot will be created,
#' depending on whether the conditional variable is continuous or categorical.
#'
#' @param dataset A dataframe.
#' @param given_var A variable from your \code{dataset} which you want to set as a condition.
#' @param n_quantiles Number of quantiles you want to partition \code{given_var} into.
#' @param var_to_plot A variable to condition on from your \code{dataset}.
#'
#' @return A ggplot graphic.
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
#' plot_single_conditional_density(variables, "V1", 5, "V2")

# Plot Single Conditional densities

# 1.Calculate the quantiles for selected variable
# 2.Plot densities of one variable based on the calculated quantiles

plot_single_conditional_density <- function(dataset, given_var, n_quantiles = 5, var_to_plot) {

  # 1.1 categorial given_var
  if (is.factor(dataset[, given_var])) {
    if (n_quantiles != length(levels(dataset[, given_var]))) {
      warning(paste0(given_var, " is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quant <- dataset[, given_var]
  } else { # 1.2 continuous given_var
    if (n_quantiles == 1) {
      data_help <- dataset
      data_help$quant <- 1
      data_help$quant <- as.factor(data_help$quant)
    } else {
      var_goal <- dplyr::select(dataset, given_var)[, 1]
      quantiles <- stats::quantile(var_goal, 1:(n_quantiles - 1) / (n_quantiles))
      quantiles <- as.numeric(quantiles)
      data_help <- dataset
      data_help$quant <- 1 + findInterval(var_goal, quantiles)
      data_help$quant <- as.factor(data_help$quant)
    }
  }

  # 2.
  g <- ggplot2::ggplot(data_help, ggplot2::aes(fill = quant)) + ggplot2::theme_minimal()

  if (is.factor(data_help[, var_to_plot]) & is.factor(data_help[, given_var])) {
    g + ggplot2::geom_bar(ggplot2::aes_string(x = var_to_plot, fill = given_var)) +
      ggplot2::ggtitle(paste0(var_to_plot, " conditional on ", given_var))
  } else if (is.factor(data_help[, var_to_plot])) {
    g + ggplot2::geom_boxplot(ggplot2::aes_string(x = var_to_plot, y = given_var)) +
      ggplot2::ggtitle(paste0(var_to_plot, " conditional on ", given_var))
  } else {
    g + ggplot2::geom_density(alpha = 0.5) + ggplot2::aes_string(x = var_to_plot) +
      ggplot2::ggtitle(paste0(var_to_plot, " conditional on ", given_var))
  }
}
