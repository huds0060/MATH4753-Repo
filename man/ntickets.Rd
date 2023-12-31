% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{ntickets}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{Number of seats on the plane}

\item{gamma}{probability of overbooking}

\item{p}{probability that a passenger will show}
}
\value{
outputs optimal number of tickets to sell while minimizing chance of overbooking
}
\description{
ntickets
}
\examples{
ntickets(200,0.02,0.95)
}
