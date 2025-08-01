% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_plots.R
\name{BA_plot_scatter}
\alias{BA_plot_scatter}
\title{Make scatter plot of paired measurements in analysis.}
\usage{
BA_plot_scatter(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE
)
}
\arguments{
\item{ba_obj}{Bland-Altman analysis object}

\item{aspect_ratio}{Set aspect ratio (x/y) between X and Y axis (sets \code{ggplot2::coord_fixed()}), Default (NULL) is automatic.}

\item{show_subject_legend}{Show legend for subjects}

\item{keep_log_scale}{Show log transformed differences. If \code{FALSE} (default), values and parameters are exponentiated before plotting}
}
\description{
Make scatter plot of paired measurements in analysis.
}
