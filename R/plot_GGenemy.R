plot_GGenemy <- function(dataset, given_var, var_to_plot = NULL, n_quantiles = 5, selfquantiles = NULL, remaining = TRUE) {
  
  if(!is.null(selfquantiles)){
      if(is.factor(dataset[, given_var])){
        data_help <- dataset
        data_help$quant <- "remaining"
        for(i in length(selfquantiles):1){
          quanthelp <- which(dataset[,given_var] == selfquantiles[i])
          data_help$quant[quanthelp] <- paste(selfquantiles[i])
        }
        if(remaining == FALSE){
          data_help  <- data_help[-which(data_help$quant == "remaining"),]
        }
      }else{
        data_help <- dataset
        data_help$quant <- "remaining"
        if(is.vector(selfquantiles)){
          if(length(selfquantiles)%%2 == 1){
            stop("Selfquantiles was not inserted correctly, with an uneven number of inserted quantile borders")
            }
          matrixquant <- matrix(selfquantiles, ncol = 2, byrow = TRUE)
          }else if(is.matrix(selfquantiles)){
            matrixquant <- selfquantiles 
            }else{
              stop("For selfquantiles: Insert a Vector of quantiles or a matrix with the quantiles ordered by row")
              }
        for(i in nrow(matrixquant):1){
          quanthelp <- which(dataset[,given_var] >= matrixquant[i,1] & dataset[,given_var] <= matrixquant[i,2])
          data_help$quant[quanthelp] <- paste(matrixquant[i,1], "to", matrixquant[i,2])
          }
    if(remaining == FALSE){
      data_help  <- data_help[-which(data_help$quant == "remaining"),]
    }
    }
    data_help$quant <- as.factor(data_help$quant)
    }else{
      if (is.factor(dataset[, given_var])) {
          if (n_quantiles != length(levels(dataset[, given_var]))) {
            warning(paste0(given_var, " is a categorical variable. The number of categories will be defined as a condition."))
          }
          data_help <- dataset
          data_help$quant <- dataset[, given_var]
        }else{
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
  
  plotit <- function(a){
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
  }
  
  if(all(c(!is.character(var_to_plot),!is.null(var_to_plot)))){
    stop(paste0(var_to_plot," has to be a character or leave it as NULL to calculate
                   conditional densities for all other variables of the dataset."))
    } else if(is.null(var_to_plot)){
      names_var <- names(dataset)
      lapply(names_var,plotit)
      
    } else if(length(var_to_plot) == 1){
      plotit(var_to_plot)
        } else {
          lapply(var_to_plot,plotit)
    }
}
