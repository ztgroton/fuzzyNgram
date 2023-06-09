% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-validate_input.R
\name{validate_input.logical}
\alias{validate_input.logical}
\title{S3 Method - Validate Format of Logical Vector Inputs}
\usage{
\method{validate_input}{logical}(
  obj,
  not_empty = FALSE,
  is_scalar = FALSE,
  allow_na = TRUE,
  check_names = FALSE,
  throw_err = TRUE,
  ...
)
}
\arguments{
\item{obj}{S3 Object}

\item{not_empty}{logical - (TRUE/FALSE) specifies if 'length(obj)' must be non-zero}

\item{is_scalar}{logical - (TRUE/FALSE) specifies if 'length(obj)' must equal 1}

\item{allow_na}{logical - (TRUE/FALSE) specifies if NA elements are allowed in 'obj'}

\item{check_names}{logical - (TRUE/FALSE) specifies if 'names(obj)' should meet same criteria as 'obj'}

\item{throw_err}{logical - (TRUE/FALSE) specifies if function should return or throw error}

\item{...}{r ellipsis}
}
\value{
logical - (TRUE/FALSE)
}
\description{
S3 Method - Validate Format of Logical Vector Inputs
}
\examples{
\dontrun{
is_valid <- validate_input.logical(obj = input, ...)
}
}
