#' Summary Statistics
#'
#'Calculates the conditional mean, variance, skewness and kurtosis for continuous variables
#'based on a range of quantiles of a given continuous variable.
#'
#'The function will return a list with one entry for each summary statistic. The 
#'list entries themselves are matrices. Per default, only conditional means will
#'be displayed. The order of the other statistics is as follows: conditional variance,
#'conditional skewness and conditional kurtosis, which will be displayed by choosing
#'n_stats to be 2, 3 or 4.
#'
#' @param dataset A data frame. Factors and logicals will be removed.
#' @param given_var A variable from your \code{dataset} which you want to set as a condition.
#' @param n_stats Number of summary statistics you want to print.  
#' @param n_quantiles Number of quantiles you want to partition \code{given_var}
#'   into, with a maximum of 10.
#'
#' @return A list with one entry for each summary statistic. 
#' @export
#'
#' @examples
#' data(iris)
#' iris <- iris[, -5]
#' plot_sum_stats(iris, "Sepal.Length", n_stats = 4, n_quantiles = 5)
plot_sum_stats <- function(dataset, given_var, n_stats=4, n_quantiles = 5, single = FALSE, whichstat = NULL){
  
  stats <- c("Mean","Variance","Skewness","Kurtosis")
  if(single == TRUE){
    if(n_stats < whichstat){
      n_stats <- whichstat
      }
    summ <- sum_stats(dataset, given_var, n_stats, n_quantiles)[[whichstat]]
    df <- data.frame(summ)
    gridExtra::marrangeGrob(
      lapply(1:length(df),function(i){
        ggplot2::ggplot(data=df,ggplot2::aes(x=1:n_quantiles,y = df[,i])) +
          ggplot2::theme_minimal() +
          ggplot2::geom_line(color=i) +
          ggplot2::geom_point() +
          ylab(paste(stats[whichstat],"of",names(df)[i])) +
          xlab(paste("Quantiles of",given_var))
        
      }),
      ncol = 3, nrow = 3, top = (paste(stats[whichstat],"of all variables given",given_var)))
    
    
  } else {
  summ <- sum_stats(dataset, given_var, n_stats, n_quantiles)
  df <- list()
  for(k in 1:n_stats){
    df[[k]] <- data.frame(summ[[k]])
  }
  lapply(1:n_stats,function(j){
    gridExtra::marrangeGrob(
  lapply(1:length(df[[1]]),function(i){
    ggplot2::ggplot(data=df[[j]],ggplot2::aes(x=1:n_quantiles,y = df[[j]][,i])) +
      ggplot2::theme_minimal() +
      ggplot2::geom_line(color=i) +
      ggplot2::geom_point() +
      ylab(paste(stats[j],"of",names(df[[j]])[i])) +
      xlab(paste("Quantiles of",given_var))
    
  }),
  ncol = 3, nrow = 3, top = (paste(stats[j],"of all variables given",given_var)))
})
  }
}
