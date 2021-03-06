% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_structure.R
\name{describe.numeric}
\alias{describe.numeric}
\title{Description of numeric variables}
\usage{
describe.numeric(x, varname = "", num.desc = c("mean", "median", "var",
  "sd", "valid.n"))
}
\arguments{
\item{x}{A numeric vector.}

\item{varname}{The variable name to display.}

\item{num.desc}{The names of the functions to apply to the vector.}
}
\value{
The vector of values returned from the functions in \code{num.desc}.
}
\description{
Describes a numeric variable.
}
\details{
\code{describe.numeric} displays the name of the vector and the results of the
functions whose names are passed in \code{num.desc}. Note that any functions that are
called by \code{describe.numeric} must have an \code{na.rm} argument, even if it is a dummy.
}
