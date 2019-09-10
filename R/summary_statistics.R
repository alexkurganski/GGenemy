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
#' sum_stats(iris, "Sepal.Length", n_stats = 4, n_quantiles = 5)
sum_stats <- function(dataset, given_var, n_stats = 1, n_quantiles = 5){
  
  # Checking for correct input of arguments
  #if(n_stats < 0 | n_stats > 4){
    #stop("You can only choose to display one to four summary statistics.")
  #}
  
  if(n_quantiles < 1 | n_quantiles > 10){
    stop("You can only partition your given_var into one to ten quantiles.")
  }
  
  if (!is.data.frame(dataset)) dataset <- as.data.frame(dataset)
  if (length(class(dataset)) > 1) {
    dataset <- unclass(dataset)
    dataset <- as.data.frame(dataset)
  }
  
  facnum <- which(sapply(dataset,is.factor))
  if(length(facnum) > 0) {
    dataset[facnum] <- NULL
    message("Factors have been removed.")
  } 
  
  lognum <- which(sapply(dataset,is.logical))
  if(length(lognum) > 0) {
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
  meanres <- matrix(nrow = n_quantiles, ncol = nnum)
  varres <- NULL
  skewres <- NULL
  kurtres <- NULL
  
  # Calculate conditional means
  if (n_stats >= 1| n_stats == "mean") {
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
  if(n_stats > 1 | n_stats == "var") {
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
  if(n_stats > 2 | n_stats == "skew") {
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
  if(n_stats > 3 | n_stats == "kurt") {
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
  stats_list <- list(meanres,
                     if(!is.null(varres)) varres,
                     if(!is.null(skewres)) skewres,
                     if(!is.null(kurtres)) kurtres)
  
  stat_names <- c("Conditional Mean",
                  "Conditional Variance",
                  "Conditional Skewness",
                  "Conditional Kurtosis")
  
  names(stats_list) <- stat_names[1:length(stats_list)]
  
  return(stats_list)
}  

# Create a nice output for the summary statistics
print_sum_stats <- function(x, given_var) {
  
  #for(i in 1:length(x)){
    #for(j in 1:length(x[[i]])) {
      #x[[i]][j] <- round(x[[i]][j], 3)
    #}
  #}
  
  nstats <-length(x)
  stat_names <- names(x)
  given_name <- names(given_var)
  cat("Given variable:", given_var, "\n")
  for(i in 1:nstats) {
    typelen <- length(x[[i]])
    if(typelen) {
      cat("\n",stat_names[i], "\n")
      print(x[[i]])
    }
  }
}


# for later
#if(plot == FALSE) {
  #return(print_sum_stats(stats_list, given_var))
#} #else {
#df <- as.data.frame(stats_list)
#df$quant <- as.factor(paste0("quantile", 1:n_quantiles))
#for(i in 1:(ncol(df)-1)) {
#ggplot2::ggplot(df, aes(x = quant, y = )) +
#ggplot::geom_col()
#}
#}