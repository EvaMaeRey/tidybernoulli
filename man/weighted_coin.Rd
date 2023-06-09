% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bernoulli-trial.R
\name{weighted_coin}
\alias{weighted_coin}
\title{Title}
\usage{
weighted_coin(prob = 0.75, outcome_set = c("tails", "heads"))
}
\arguments{
\item{prob}{probability of success, default is .25}
}
\value{
probability and outcome as 0 and 1
}
\description{
Title
}
\examples{
weighted_coin()
weighted_coin(prob = .55)
weighted_coin(outcome_set = 0:1)
weighted_coin(outcome_set = c(FALSE, TRUE))
weighted_coin(outcome_set = c("fish", "no fish"))
}
