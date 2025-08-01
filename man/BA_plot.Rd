% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_plots.R
\name{BA_plot}
\alias{BA_plot}
\title{Create Bland-Altman plot}
\usage{
BA_plot(
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
\value{
Bland-Altman plot (ggplot)
}
\description{
Creates a standard Bland-Altman plot from a Bland-Altman analysis object made with \code{\link[=compare_methods]{compare_methods()}}.
}
\examples{
ba_obj <- compare_methods(CO, ic, rv, id_col = sub)
BA_plot(ba_obj)

}
\seealso{
\code{\link[=BA_plot_normalized_log]{BA_plot_normalized_log()}} which shows the results of a proportional Bland-Altman analysis (with log-transformed measurements)
on the non-transformed data.
}
