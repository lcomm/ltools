% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addDimension.R
\name{addDimension}
\alias{addDimension}
\title{Add a dimension based on an existing object}
\usage{
addDimension(x, n, value = NULL)
}
\arguments{
\item{x}{Scalar, vector, matrix, or array to extend}

\item{n}{Length of the dimension to be added}

\item{value}{Values with which to fill the new object. If no values are 
provided, defaults to filling repeating the values of x.}
}
\value{
An object of one more dimension than x
}
\description{
Add a dimension based on an existing object
}

