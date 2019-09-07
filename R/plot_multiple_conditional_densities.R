#' Plot multiple conditional densities 
#'
#' Plots densities of chosen continuous variables, conditioned on quantiles of
#' continuous variables or categories of categorical variables. For categorical
#' variables, boxplots or barplots will be created, depending on whether the
#' variable to be conditioned on is continuous or categorical.
#'
#'
#' @param dataset A dataframe.
#' @param given_var A variable from your \code{dataset} which you want to set as a condition.
#' @param n_quantiles Number of quantiles you want to partition \code{given_var} into.
#' @param var_to_plot Vector of variables from your \code{dataset} which you want to plot.
#'
#' @return Several ggplot graphics. The total number of plots will equal the
#'   number of variables in your \code{dataset}.
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
#' plot_multiple_conditional_densities(variables, "V1", n_quantiles = 5, c("V2","V3"))

# Plot multiple Conditional densities

# 1.Calculate the quantiles for selected variable 
# 2.Plot densities of chosen other variables based on the calculated quantiles
plot_multiple_conditional_densities <- function(dataset, given_var, n_quantiles = 5, var_to_plot) {
  
  # 1.1 categorical given_var
  if (is.factor(dataset[, given_var])) {
    if (n_quantiles != length(levels(dataset[, given_var]))) {
      warning(paste0(given_var, " is a categorical variable. The number of categories will be defined as a condition."))
    }
    data_help <- dataset
    data_help$quant <- dataset[, given_var]
  } else { # 1.2 continuous given_var
    var_goal <- dplyr::select(dataset, given_var)[, 1]
    quantiles <- stats::quantile(var_goal, 1:(n_quantiles - 1) / (n_quantiles))
    quantiles <- as.numeric(quantiles)
    data_help <- dataset
    data_help$quant <- 1 + findInterval(var_goal, quantiles)
    data_help$quant <- as.factor(data_help$quant)
  }
  
  # 2.
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
