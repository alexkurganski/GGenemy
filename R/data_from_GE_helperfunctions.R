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
