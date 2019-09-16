# Note: These are functions from Jim Lemon's package prettyR.
# We wanted to use the describe() function, but had to alter the describe.factor()
# function to create a fitting output for our Shiny app.

# Function to use in describe.factor() and describe.numeric()
# Not to be exported
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

describe.factor <- function(x, varname = "",
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

#' Description of variables
#'
#' Describes a vector or the columns in a matrix or data frame.
#'
#' describe displays a table of descriptive statistics for numeric, factor and
#' logical variables in the object x. The summary measures for numeric variables
#' can easily be altered with the argument num.desc. Pass a character vector with
#' the names of the desired summary measures and these will be displayed at the
#' top of the numeric block with their results beneath them. If quantiles are
#' desired, the user will have to write wrapper functions that call quantile with
#' the appropriate type or probabilities and use the names of the wrapper
#' functions in num.desc. Remember that any function called by describe must have
#' an na.rm argument.
#'
#' Percentages are now always displayed and returned in the tables for factor and
#' logical variables.
#'
#' @param x A vector, matrix or data frame.
#' @param num.desc The names of the functions to apply to numeric data.
#' @param xname A name for the object x, mostly where this would be a very long
#' string describing its structure (e.g. if it was extracted by name from a data
#' frame).
#'
#' @return A list with three components:
#'
#' \code{Numeric}	 A list of the values returned from describe.numeric for
#' each column described.
#'
#' \code{Factor}   A list of the tables for each column described.
#'
#' \code{Logical}  A list of the tables for each column described.
#' @export
describe <- function(x, num.desc = c("mean", "median", "var", "sd", "valid.n"),
                     xname = NA) {
  if (missing(x)) {
    stop("Usage: describe(x,...) where x is a vector, data frame or matrix")
  }
  if (!is.data.frame(x)) x <- as.data.frame(x)
  varnames <- names(x)
  if (is.null(varnames)) varnames <- paste("V", 1:dim(x)[2], sep = "")
  if (is.data.frame(x)) {
    if (is.na(xname)) xname <- deparse(substitute(x))
    #cat("Description of", xname, "\n")
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
          varname = varnames[fac.index[facres]]
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


print.desc <- function(x, ndec = 2, ...) {
  desclen <- length(x)
  descnames <- names(x)
  for (desctype in 1:desclen) {
    typelen <- length(x[[desctype]])
    if (typelen) {
      cat("\n", descnames[desctype], "\n")
      nvar <- length(x[[desctype]])
      if (descnames[desctype] == "Numeric") {
        nrows <- length(x[[desctype]])
        ncols <- length(x[[desctype]][[1]])
        xmat <- matrix(round(unlist(x[[desctype]]), ndec),
          nrow = nrows, ncol = ncols, byrow = TRUE
        )
        colnames(xmat) <- names(x[[desctype]][[1]])
        rownames(xmat) <- names(x[[desctype]])
        print(xmat)
      }
      else {
        for (descvar in 1:nvar) {
          print(round(x[[desctype]][[descvar]], 2))
          xmax <- max(x[[desctype]][[descvar]][1, ])
          nmax <- sum(x[[desctype]][[descvar]][1, ] == xmax)
          cat("Mode:", ifelse(nmax > 1, "more than one mode",
            names(which.max(x[[desctype]][[descvar]][1, ]))
          ), "\n")
        }
      }
    }
  }
}
