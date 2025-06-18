#' Print method for ba_analysis objects
#'
#' @param x BA analysis object.
#' @param ... not used.
#'
#' @export
print.ba_analysis <- function(x, ...) {
  ops <- options(digits = 3)
  on.exit(options(ops))

  n_obs <- x$diff_model@devcomp$dims[["n"]]
  n_sub <- nlevels(x$diff_model@flist[[1]])

  txt_logtrans <- if (attr(x, "logtrans")) "(log transformed) " else ""

  cat(glue::glue(
    "{n_obs} paired measurements {txt_logtrans}in {n_sub} subjects\n\n\n"
  ))

  # Create label for CI if CI exists
  if (is.null(x$BA_stats_ci)) {
    CI_label <- NULL
  } else {
    CI_label <- sprintf(
      "     [%2g%% CI]",
      attr(x$BA_stats_ci, "level") * 100
    )
  }

  # Function that formats a single line of results
  format_line <- function(label, est, ci.lwr, ci.upr) {
    if (is.null(est)) {
      return()
    }
    cat(
      format(label, width = 30),
      ": ",
      sprintf("% 2.3f", est),
      sprintf("[% 2.3f; % 2.3f]", ci.lwr, ci.upr),
      "\n"
    )
  }

  format_line_stat <- function(label, var) {
    format_line(
      label,
      x$BA_stats[[var]],
      x$BA_stats_ci[[var]][1],
      x$BA_stats_ci[[var]][2]
    )
  }

  cat(format("", width = 30), "    est", CI_label, "\n")
  cat("=== Distribution ===\n")
  format_line_stat("Mean", "distr.mean")
  format_line_stat("Between-subject variation (SD)", "distr.sd.between")
  format_line_stat("Within-subject variation (SD)", "distr.sd.within")
  format_line_stat("Total variation (SD)", "distr.sd.total")
  cat("\n")
  cat("=== Method comparison ===\n")
  format_line_stat("Bias (alt - ref)", "bias")
  format_line_stat("Between-subject variation (SD)", "sd.between")
  format_line_stat("Within-subject variation (SD)", "sd.within")
  format_line_stat("Total variation (SD)", "sd.total")
  cat("Intraclass correlation\n")
  format_line_stat("  Between/Total variance", "intraclass.correlation")
  cat("\n")
  cat("Limits of agreement (95%)\n")
  format_line_stat("\u251C Upper limit", "loa.upr")
  format_line_stat("\u2514 Lower limit", "loa.lwr")
  cat("\n")
  format_line_stat("Percentage error", "percentage.error")
  cat("\n")
  cat("--- Trending ---\n")
  format_line_stat("Within-subject perc. error", "percentage.error.within")
  format_line_stat("Change LoA [±] (95%)", "change.loa")

  invisible(x)
}
