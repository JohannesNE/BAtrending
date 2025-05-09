% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_plots.R
\name{plot_normalized_log_BA}
\alias{plot_normalized_log_BA}
\title{Plot BA estimates from log transformed data on raw data.}
\usage{
plot_normalized_log_BA(ba_obj, show_subject_legend = FALSE)
}
\arguments{
\item{ba_obj}{BA analysis object}

\item{show_subject_legend}{Show legend for subjects}
}
\value{
Bland Altman style plot with relative differences plotted on absolute differences.
}
\description{
Plot BA estimates from log transformed data on raw data.
}
\examples{
plot_BA(compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE))

}
