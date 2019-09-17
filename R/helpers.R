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

# Helper function to load ggplot objects into the global environment

assign_to_global <- function(input, value, pos=1){
  assign(paste0("GGenemyPlot",input), value = value,
         envir = as.environment(pos))
}

#
