% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPostMeanMedCI.R
\name{getPostMeanMedCI}
\alias{getPostMeanMedCI}
\title{Make table of posterior mean, median, and credible interval}
\usage{
getPostMeanMedCI(getPostMeanMedCI.mat, alpha = 0.05)
}
\arguments{
\item{getPostMeanMedCI.mat}{Matrix whose columns contain MCMC draws}

\item{alpha}{Type I error rate for credible interval.  Defaults to 0.05.}
}
\description{
Function to calculate posterior mean, median, and (1-alpha)x100% credible
interval table based on a matrix with columns as parameters' MCMC draws
}
\examples{
## Calculate mean, median, and 95\% quantile-based interval for columns
my.matrix = matrix(rnorm(5000,rep(1:5,each=1000),0.5),1000,5)
getPostMeanMedCI(my.matrix)

}
\seealso{
makeCIs, getMeanSDCI
}
\keyword{posterior}
\keyword{summary}

