% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_stats.R
\name{add_confint}
\alias{add_confint}
\title{Add confidence intervals to BA analysis object.}
\usage{
add_confint(ba_obj, level = 0.95, nsim = 1999, .progress = "txt")
}
\arguments{
\item{ba_obj}{BA analysis object}

\item{level}{Confidence level (default is 0.95)}

\item{nsim}{Number of bootstrap samples}

\item{.progress}{see `?lme4::bootMer()`
``}
}
\value{
BA analysis object (`x`) with added confidence intervals
}
\description{
Add confidence intervals to BA analysis object.
}
