% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{wtVCosSimilarity}
\alias{wtVCosSimilarity}
\title{Compute weighted cosine similarity of two numeric vectors}
\usage{
wtVCosSimilarity(a, b, w)
}
\arguments{
\item{a}{numeric vector}

\item{b}{numeric vector}

\item{w}{numeric vector of weights}
}
\value{
Numeric value of cosine similarity
}
\description{
Compute weighted cosine similarity of two numeric vectors
}
\details{
\code{a}, \code{b} and \code{c} are expected to have the same length.
\deqn{\sum(a*b*w)/\sqrt\sum(w*a^2)\sqrt\sum(w*b^2)}
}
