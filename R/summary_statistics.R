#' Summary Statistics
#'
#' Calculates the conditional mean, variance, skewness and kurtosis for continuous variables
#' based on a set of quantiles of a given continuous variable.
#'
#' The function will return a list with one entry for each summary statistic. The
#' list entries themselves are matrices. Per default, only conditional means will
#' be displayed. You can choose the statistics you want to print with the input of
#' \code{stats}. You can either name the statistics as a choice of \code{stats =
#' c("mean", "var", "skewness", "kurtosis")} or as numbers \code{stats = c(1, 2,
#' 3, 4)}, which represent the single statistics.
#'
#' @param dataset A data frame. Factors and logicals will be removed.
#' @param given_var A variable from your \code{dataset} which you want to set as a condition.
#' @param stats A vector with the choice of summary statistics you want to print.
#' @param n_quantiles Number of quantiles you want to partition \code{given_var}
#'   into, with a maximum of 10.
#'
#' @return A list with one entry for each summary statistic.
#' @export
#'
#' @examples
#' data(iris)
#' iris <- iris[, -5]
#' sum_stats(iris, "Sepal.Length", stats = 4, n_quantiles = 5)
sum_stats <- function(dataset, given_var, stats = 1, n_quantiles = 5) {
  if (n_quantiles < 1 | n_quantiles > 10) {
    stop("You can only partition your given_var into one to ten quantiles.")
  }

  if (!is.data.frame(dataset)) dataset <- as.data.frame(dataset)
  if (length(class(dataset)) > 1) {
    dataset <- unclass(dataset)
    dataset <- as.data.frame(dataset)
  }

  facnum <- which(sapply(dataset, is.factor))
  if (length(facnum) > 0) {
    dataset[facnum] <- NULL
    message("Factors have been removed.")
  }

  lognum <- which(sapply(dataset, is.logical))
  if (length(lognum) > 0) {
    dataset[lognum] <- NULL
    message("Logicals have been removed.")
  }

  # 1. Create quantiles for given_var
  if (n_quantiles == 1) { # only one quantile
    data_help <- dataset
    data_help$quant <- 1
    data_help$quant <- as.factor(data_help$quant)
  } else { # more than one quantile
    var_goal <- dplyr::select(dataset, given_var)[, 1]
    quantiles <- stats::quantile(var_goal, 1:(n_quantiles - 1) / (n_quantiles))
    quantiles <- as.numeric(quantiles)
    data_help <- dataset
    data_help$quant <- 1 + findInterval(var_goal, quantiles)
    data_help$quant <- as.factor(data_help$quant)
  }


  # 2. Calculate summary statistics

  varnames <- names(dataset)
  if (is.null(varnames)) varnames <- paste("V", 1:dim(dataset)[2], sep = "")

  num.index <- which(sapply(dataset, is.numeric))
  nnum <- length(num.index)

  # Setting up outputs
  meanres <- NULL
  varres <- NULL
  skewres <- NULL
  kurtres <- NULL

  # Calculate conditional means
  if (1 %in% stats | "mean" %in% stats) {
    meanres <- matrix(nrow = n_quantiles, ncol = nnum)

    for (i in 1:n_quantiles) {
      for (j in 1:nnum) {
        sub <- subset(data_help, quant == as.character(i), select = varnames[j])
        meanres[i, j] <- mean(sub[, 1])
      }
    }

    colnames(meanres) <- varnames[num.index]
    rownames(meanres) <- paste0("quantile", 1:n_quantiles)
  }

  # Calculate conditional variances
  if (2 %in% stats | "var" %in% stats) {
    varres <- matrix(nrow = n_quantiles, ncol = nnum)

    for (i in 1:n_quantiles) {
      for (j in 1:nnum) {
        sub <- subset(data_help, quant == as.character(i), select = varnames[j])
        varres[i, j] <- var(sub[, 1])
      }
    }

    colnames(varres) <- varnames[num.index]
    rownames(varres) <- paste0("quantile", 1:n_quantiles)
  }

  # Calculate conditional skewness
  if (3 %in% stats | "skewness" %in% stats) {
    skewres <- matrix(nrow = n_quantiles, ncol = nnum)

    for (i in 1:n_quantiles) {
      for (j in 1:nnum) {
        sub <- subset(data_help, quant == as.character(i), select = varnames[j])
        skewres[i, j] <- moments::skewness(sub[, 1])
      }
    }

    colnames(skewres) <- varnames[num.index]
    rownames(skewres) <- paste0("quantile", 1:n_quantiles)
  }

  # Calculate conditional kurtosis
  if (4 %in% stats | "kurtosis" %in% stats) {
    kurtres <- matrix(nrow = n_quantiles, ncol = nnum)

    for (i in 1:n_quantiles) {
      for (j in 1:nnum) {
        sub <- subset(data_help, quant == as.character(i), select = varnames[j])
        kurtres[i, j] <- moments::kurtosis(sub[, 1])
      }
    }

    colnames(kurtres) <- varnames[num.index]
    rownames(kurtres) <- paste0("quantile", 1:n_quantiles)
  }
  # Collect results in a list
  stats_list <- list(
    "Cond. Mean" = meanres,
    "Cond. Variance" = varres,
    "Cond. Skewness" = skewres,
    "Cond. Kurtosis" = kurtres
  )

  stats <- c(
    "Cond. Mean", "Cond. Variance", "Cond. Skewness",
    "Cond. Kurtosis"
  )

  # remove empty lists
  for (i in stats) {
    if (is.null(stats_list[[i]])) {
      stats_list[[i]] <- NULL
    }
  }
  
  for (k in 1:length(stats_list)) {
    stats_list[[k]] <- data.frame(stats_list[[k]])
  }

  return(stats_list)
}

# Create a nice output for the summary statistics



#' Print results of \code{sum_stats()} nicely
#'
#' Use this function to print your results from \code{sum_stats()} in a more
#' reader-friendly way.
#'
#' @param x An object created by sum_stats().
#' @param given_var The given variable you used to create \code{x}.
#'
#' @return The same results as before, but printed nicely.
#' @export
#'
print_sum_stats <- function(x, given_var) {
  
  #cat("Given variable:", given_var, "\n")
  #for (i in 1:length(x)) {
    #print(DT::datatable(x[[i]], caption = htmltools::tags$caption(
      #htmltools::strong(names(x)[i]))))
  #}
  nstats <- length(x)
  stat_names <- names(x)
  given_name <- names(given_var)
  cat("Given variable:", given_var, "\n")
  for (i in 1:nstats) {
    typelen <- length(x[[i]])
    if (typelen) {
      cat("\n", stat_names[i], "\n")
      print(x[[i]])
    }
  }
}
