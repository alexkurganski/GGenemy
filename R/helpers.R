# Helper functions to insert into prettyR::describe(num.desc = c("..."))

# Wrapper for quantiles
quantile0.25 <- function(data, na.rm = TRUE) {
  if (base::is.data.frame(data)) {
    out <- rep(NA, ncol(data))
    for (i in 1:ncol(data)) {
      if (base::is.numeric(data[, i])) {
        out[i] <- stats::quantile(data[, i], probs = 0.25, na.rm = TRUE)
      }
    }
  } else if (base::is.numeric(data)) {
    out <- stats::quantile(data, probs = 0.25, na.rm = TRUE)
  } else {
    out <- NA
  }
  return(out)
}

quantile0.75 <- function(data, na.rm = TRUE) {
  if (base::is.data.frame(data)) {
    out <- rep(NA, ncol(data))
    for (i in 1:ncol(data)) {
      if (base::is.numeric(data[, i])) {
        out[i] <- stats::quantile(data[, i], probs = 0.75, na.rm = TRUE)
      }
    }
  } else if (base::is.numeric(data)) {
    out <- stats::quantile(data, probs = 0.75, na.rm = TRUE)
  } else {
    out <- NA
  }
  return(out)
}


# Helper function to read in data frames from the global environment

#' Internal: Seatch Dataframe in GE
#'
#' Searches for Dataframes in the Global Environment.
#' @keywords internal

search_dataframe <- function() {
  # Get components of Global Environment
  namesGE <- ls(envir = .GlobalEnv)
  
  # Look for bamlss & gamlss components
  dataframe_true <- sapply(namesGE,check_dataframe)
  
  # Return nothing if no bamlss or gamlss
  if (length(dataframe_true) > 0)
    if (sum(dataframe_true) > 0)
      return(namesGE[dataframe_true])
  else
    return("")
}

#' Internal: Dataframe Checker
#'
#' Check whether the insertet object is a dataframe or not.
#' @keywords internal
check_dataframe <- function(x) {
  if (is.character(x))
    obj <- get(x, envir = .GlobalEnv)
  else
    obj <- x
  if (is.data.frame(obj))
    return(TRUE)
  else
    return(FALSE)
}


# Helper function to load ggplot objects into the global environment

assign_to_global1 <- function(input, value, pos=1){
  assign(paste0("GGenemy_Condplot",input), value = value,
         envir = as.environment(pos))
}

assign_to_global2 <- function(input, value, pos=1){
  assign(paste0("GGenemy_SelfRangeplot",input), value = value,
         envir = as.environment(pos))
}

assign_to_global3 <- function(input, value, pos=1){
  assign(paste0("GGenemy_Sumstatsplot",input), value = value,
         envir = as.environment(pos))
}


# Helper function to remove variables with only one value from the choices for
# conversion to a factor

unilen <- function(x){
  unilen <- length(unique(x))
  if (unilen == 1){
    res <- TRUE
  } else {
    res <- FALSE
  }
  return(res)
}

