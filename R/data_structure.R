# Note: These are functions from Jim Lemon's package prettyR.
# We wanted to use the describe() function, but had to alter the describe.factor()
# function to create a fitting output for our Shiny app.

Mode <- function(x, na.rm = FALSE) {
  xtab <- table(x, useNA = ifelse(na.rm, "no", "ifany"))
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

valid.n <- function(x, na.rm = TRUE) {
  return(ifelse(na.rm, sum(!is.na(x)), length(x)))
}

#' Description of numeric variables
#' 
#' Describes a numeric variable.
#' 
#' \code{describe.numeric} displays the name of the vector and the results of the 
#' functions whose names are passed in \code{num.desc}. Note that any functions that are
#' called by \code{describe.numeric} must have an \code{na.rm} argument, even if it is a dummy.
#'
#' @param x A numeric vector.
#' @param varname The variable name to display.
#' @param num.desc The names of the functions to apply to the vector.
#'
#' @return The vector of values returned from the functions in \code{num.desc}.
#' 
#' @import stats
describe.numeric <- function(x, varname = "",
                             num.desc = c("mean", "median", "var", "sd", "valid.n")) {
  desclen <- length(num.desc)
  desc.vector <- rep(0, desclen)
  if (nzchar(varname) == 0) varname <- deparse(substitute(x))
  for (i in 1:desclen) {
    if (valid.n(x)) {
      desc.vector[i] <- do.call(num.desc[i], list(x, na.rm = TRUE))
    } else {
      desc.vector[i] <- NA
    }
  }
  names(desc.vector) <- num.desc
  return(desc.vector)
}

describe.factor <- function(x, varname = "", horizontal = FALSE,
                            decr.order = TRUE) {
  if (nzchar(varname) == 0) varname <- deparse(substitute(x))
  lenx <- length(x)
  factab <- table(x, useNA = "ifany")
  factab <- rbind(factab, 100 * factab / sum(factab))
  dimnames(factab)[[1]] <- c("Count", "Percent")
  names(dimnames(factab)) <- c(varname, "")
  if (decr.order) factab <- factab[, order(factab[1, ], decreasing = TRUE)]
  if (dim(factab)[2] > 10) {
    factab <- factab[, 1:10]
  }
  return(factab)
}

describe.logical <- function(x, varname = "") {
  if (nzchar(varname) == 0) varname <- deparse(substitute(x))
  cat(varname, "\n")
  nmiss <- sum(is.na(x))
  if (all(is.na(x))) {
    logjam <- c(0, 0)
  } else {
    logjam <- table(x, useNA = "ifany")
  }
  logjam <- rbind(logjam, 100 * logjam / sum(logjam))
  dimnames(logjam)[[1]] <- c("Count", "Percent")
  return(logjam)
}

describe <- function(x, num.desc = c("mean", "median", "var", "sd", "valid.n"),
                     xname = NA, horizontal = FALSE) {
  if (missing(x)) {
    stop("Usage: describe(x,...)\n\twhere x is a vector, data frame or matrix")
  }
  if (!is.data.frame(x)) x <- as.data.frame(x)
  varnames <- names(x)
  if (is.null(varnames)) varnames <- paste("V", 1:dim(x)[2], sep = "")
  if (is.data.frame(x)) {
    if (is.na(xname)) xname <- deparse(substitute(x))
    cat("Description of", xname, "\n")
    num.index <- which(sapply(x, is.numeric))
    nnum <- length(num.index)
    num.result <- list()
    if (nnum) {
      for (numres in 1:nnum) {
        num.result[[numres]] <-
          describe.numeric(x[[num.index[numres]]],
            num.desc = num.desc,
            varname = varnames[num.index[numres]]
          )
      }
      names(num.result) <- varnames[num.index]
    }
    fac.index <- c(which(sapply(x, is.factor)), which(sapply(x, is.character)))
    nfac <- length(fac.index)
    fac.result <- list()
    if (nfac) {
      for (facres in 1:nfac) {
        fac.result[[facres]] <- describe.factor(x[[fac.index[facres]]],
          varname = varnames[fac.index[facres]], horizontal = horizontal
        )
      }
      names(fac.result) <- varnames[fac.index]
    }
    log.index <- which(sapply(x, is.logical))
    nlog <- length(log.index)
    log.result <- list()
    if (nlog) {
      for (logres in 1:nlog) {
        log.result[[logres]] <- describe.logical(x[[log.index[logres]]],
          varname = varnames[log.index[logres]]
        )
      }
      names(log.result) <- varnames[log.index]
    }
    desc.list <- list(Numeric = num.result, Factor = fac.result, Logical = log.result)
    class(desc.list) <- "desc"
    return(desc.list)
  }
  else {
    cat("describe: x must be a vector, matrix or data frame\n")
  }
}
