#' Plot GGenemy
#'
#' Plots densities of continuous variables, conditioned on quantiles of
#' continuous variables or categories of categorical variables. For categorical
#' variables, boxplots or barplots will be created, depending on whether the
#' variable to be conditioned on is continuous or categorical.
#' Either the user can use a number of same sized quantiles or
#' can select the size of own quantiles
#'
#'
#' @param dataset A dataframe.
#' @param given_var A variable from your \code{dataset}.
#' @param var_to_plot The variables to plot. When not selecting any variable, all variables from your \code{dataset}
#'   will be plotted.
#' @param n_quantiles Number of quantiles you want to partition \code{given_var}
#'   into.
#' @param boxplot logical. If TRUE boxplots will be presented instead of densities
#' Also possible to use a vector with the names of variables, which should be plotted as boxplots instead of the density.
#' @param selfquantiles Vector of n times two Values or matrix with 2 columns.
#' The first value as min and the second value as max of the self selected quantile.
#' @param remaining logical. If TRUE the remaining values not within selfquantiles will be plotted to
#' category remaining.
#'
#'
#'
#' @return Several ggplot graphics. The total number of plots will equal the
#'   number of variables in your \code{dataset} or selected variables from \code{var_to_plot}
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
#'
#' plot_GGenemy(variables, "V1")
#'
#' # Plot GGenemy
#'
#' # 1.Calculate the quantiles for selected variable
#' # 2.Plot densities of all or selected variables based on the calculated quantiles

plot_GGenemy <- function(dataset, given_var, var_to_plot = NULL, n_quantiles = 5,
                         boxplot = FALSE, selfquantiles = NULL, remaining = TRUE) {

  if (length(class(dataset)) > 1) {
    dataset <- unclass(dataset)
    dataset <- as.data.frame(dataset)
  }
  
  if (!is.null(selfquantiles)) {
    if (is.factor(dataset[, given_var])) {
      data_help <- dataset
      data_help$quant <- "remaining"
      for (i in length(selfquantiles):1) {
        quanthelp <- which(dataset[, given_var] == selfquantiles[i])
        data_help$quant[quanthelp] <- paste(selfquantiles[i])
      }
      if (remaining == FALSE) {
        data_help <- data_help[-which(data_help$quant == "remaining"), ]
      }
    } else if (is.matrix(selfquantiles) | length(selfquantiles)%%2 == 0) {
      if(is.matrix(selfquantiles)){
        matrixquant <- selfquantiles
      } else {
        matrixquant <- matrix(selfquantiles, ncol = 2, byrow = TRUE)
      }
      data_help <- dataset
      data_help$quant <- "remaining"
      for (i in nrow(matrixquant):1) {
        quanthelp <- which(dataset[, given_var] >= matrixquant[i, 1] & dataset[, given_var] <= matrixquant[i, 2])
        helper <- duplicated(as.vector(t(matrixquant)))[c(-1,0)+i*2]
        if(any(helper)){
          if(helper[1] & !all(helper)){
            data_help$quant[quanthelp] <- paste(">",matrixquant[i, 1], "to", "<=",matrixquant[i, 2])
          } else if(helper[2] & !all(helper)){
            data_help$quant[quanthelp] <- paste(">=",matrixquant[i, 1], "to", "<",matrixquant[i, 2])
          } else {
            data_help$quant[quanthelp] <- paste(">",matrixquant[i, 1], "to", "<",matrixquant[i, 2])
          }
        }else{
        data_help$quant[quanthelp] <- paste(">=",matrixquant[i, 1], "to", "<=",matrixquant[i, 2])
        }
      }
      if (remaining == FALSE) {
        data_help <- data_help[-which(data_help$quant == "remaining"), ]
      }
    } else {
      stop("For selfquantiles: Insert a Vector of quantiles or a matrix with the quantiles ordered by row")
    }
    
    if (remaining == FALSE) {
      data_help <- data_help[-which(data_help$quant == "remaining"), ]
    }
    data_help$quant <- as.factor(data_help$quant)
  } else {
    if (is.factor(dataset[, given_var])) {
      if (n_quantiles != length(levels(dataset[, given_var]))) {
        message(paste0(given_var, " is a categorical variable. The number of categories will be defined as a condition."))
      }
      data_help <- dataset
      data_help$quant <- dataset[, given_var]
    } else {
      var_goal <- dplyr::select(dataset, given_var)[, 1]
      quantiles <- stats::quantile(var_goal, 1:(n_quantiles - 1) / (n_quantiles))
      quantiles <- as.numeric(quantiles)
      data_help <- dataset
      data_help$quant <- 1 + findInterval(var_goal, quantiles)
      data_help$quant <- as.factor(data_help$quant)
      for(i in 1:n_quantiles){
        if(i == 1){
          levels(data_help$quant)[i] <- paste(round(min(var_goal),2),"to",round(quantiles[i],2))
        } else if(!(i == length(levels(data_help$quant)))){
          levels(data_help$quant)[i] <- paste(round(quantiles[i-1],2),"to",round(quantiles[i],2))
        } else {
          levels(data_help$quant)[i] <- paste(round(quantiles[i-1],2),"to",round(max(var_goal),2))
        }
      }
    }
  }
  
  # 2.
  g <- ggplot2::ggplot(data_help, ggplot2::aes(fill = quant)) + ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15, face = "bold"))
  
  plotit <- function(a) {
    if (is.factor(data_help[, a]) & is.factor(data_help[, given_var])) {
      g + ggplot2::geom_bar(ggplot2::aes_string(x = a, fill = given_var)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else if (is.factor(data_help[, a])) {
      g + ggplot2::geom_boxplot(ggplot2::aes_string(x = a, y = given_var)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else if (!is.logical(boxplot) & any(boxplot == a)) {
      g + ggplot2::geom_boxplot(ggplot2::aes_string(x = given_var, y = a)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else if (any(boxplot == TRUE)) {
      g + ggplot2::geom_boxplot(ggplot2::aes_string(x = given_var, y = a)) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    } else {
      g + ggplot2::geom_density(alpha = 0.5) + ggplot2::aes_string(x = a) +
        ggplot2::ggtitle(paste0(a, " conditional on ", given_var))
    }
  }
  
  if (all(c(!is.character(var_to_plot), !is.null(var_to_plot)))) {
    stop(paste0(var_to_plot, " has to be a character or leave it as NULL to calculate
                   conditional densities for all other variables of the dataset."))
  } else if (is.null(var_to_plot)) {
    names_var <- names(dataset)
    lapply(names_var, plotit)
  } else if (length(var_to_plot) == 1) {
    plotit(var_to_plot)
  } else {
    lapply(var_to_plot, plotit)
  }
}
