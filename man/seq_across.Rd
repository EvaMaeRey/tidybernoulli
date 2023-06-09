% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bernoulli-trial.R
\name{seq_across}
\alias{seq_across}
\title{Title}
\usage{
seq_across(data, var_key = "outcome")
}
\arguments{
\item{var_key}{}
}
\description{
Title
}
\examples{
library(magrittr)
library(dplyr)
bernoulli_trial(prob = .5) \%>\%
add_trials(3) \%>\%
  .$out \%>\%
  seq_across() \%>\%
  prod_across() \%>\%
  group_by(global_outcome) \%>\%
  summarize(probs = sum(global_probs))

library(magrittr)
library(dplyr)
bernoulli_trial(prob = .25) \%>\%
add_trials(3) \%>\%
  .$out \%>\%
  seq_across() \%>\%
  prod_across()
}
