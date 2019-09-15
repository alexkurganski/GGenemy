#' Plot Summary Statistics
#'
#' Calculates the conditional mean, variance, skewness and kurtosis for continuous variables
#' based on a range of quantiles of a given continuous variable and displays them
#' as line plots.
#'
#' The function will return one plot for each of the chosen summary statistics. The
#' values will be displayed as line plots, each point representing the value of
#' the statistic conditioned on a specific quantile of the given variable.
#'
#' @param dataset A data frame. Factors and logicals will be removed.
#' @param given_var A variable from your \code{dataset} which you want to set as a condition.
#' @param stats Vector of summary statistics you want to plot.
#' @param n_quantiles Number of quantiles you want to partition \code{given_var}
#'   into, with a maximum of 10.
#'
#' @return One plot for each statistic, showing all variables conditioned on
#' the given variable.
#' @export
#'
#' @examples
#' data(iris)
#' iris <- iris[, -5]
#' plot_sum_stats(iris, "Sepal.Length", stats = c(1, 2, 3, 4), n_quantiles = 5)
plot_sum_stats <- function(dataset, given_var, stats = 1, n_quantiles = 5) {
  summ <- sum_stats(dataset, given_var, stats, n_quantiles)
  df <- list()
  for (k in 1:length(summ)) {
    df[[k]] <- data.frame(summ[[k]])
  }
  lapply(1:length(summ), function(j) {
    gridExtra::marrangeGrob(
      lapply(1:length(df[[1]]), function(i) {
        ggplot2::ggplot(data = df[[j]], ggplot2::aes(x = 1:n_quantiles, y = df[[j]][, i])) +
          ggplot2::theme_minimal() +
          ggplot2::geom_line(color = i, size = 2.5) +
          ggplot2::geom_point(size = 2.5) +
          ggplot2::ylab(paste(names(summ[j]), "of", names(df[[j]])[i])) +
          ggplot2::xlab(paste("Quantiles of", given_var))
      }),
      ncol = 3, nrow = 3, top = (paste(names(summ[j]), "of all variables given", given_var))
    )
  })
}
