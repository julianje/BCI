\name{BCI-package}
\alias{BCI-package}
\alias{BCI}
\docType{package}
\title{
\packageTitle{BCI}
}
\description{
\packageDescription{BCI}
}
\details{

\packageDESCRIPTION{BCI}
\packageIndices{BCI}
BCI let's you get a vector's confidence intervals through CI(), a lower bound with LowerCI(), and an upper bound with TopCI().
}
\author{
\packageAuthor{BCI}

Maintainer: \packageMaintainer{BCI}
}
\references{
see boot documentation.
}
\examples{
library(datasets)
library(dplyr)
airquality \%>\% group_by(Month) \%>\%
summarise(MeanWind=mean(Wind),
	      LowerBound=LowerCI(Wind),
	      UpperBound=TopCI(Wind))
}
