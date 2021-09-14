#' Print method
#' @export
print.ba_analysis <- function(ba_obj) {
    ops <- options(digits = 3)
    on.exit(options(ops))

    n_obs <- ba_obj$model@devcomp$dims[["n"]]
    n_sub <- nlevels(ba_obj$model@flist[[1]])

    txt_logtrans <- if (attr(ba_obj, "logtrans")) "(log transformed) " else ""

    cat(glue::glue("{n_obs} paired measurements {txt_logtrans}in {n_sub} subjects\n\n\n"))

    # Create label for CI if CI exists
    if (is.null(ba_obj$BA_stats_ci)) {
        CI_label <- NULL
    }
    else {
        CI_label <- sprintf("     [%2g%% CI]", attr(ba_obj$BA_stats_ci, "level") * 100)
    }

    # Function that formats a single line of results
    format_line <- function(label, est, ci.lwr, ci.upr) {
        if (is.null(est)) {
            return()
        }
        cat(format(label, width = 29), ": ",
            sprintf("% 2.3f", est),
            sprintf("[% 2.3f; % 2.3f]", ci.lwr, ci.upr), "\n")
    }

    format_line_stat <- function(label, var) {
        format_line(label,
                    ba_obj$BA_stats[[var]],
                    ba_obj$BA_stats_ci[[var]][1],
                    ba_obj$BA_stats_ci[[var]][2])
    }

    cat(format("", width = 30), "    est", CI_label, "\n")
    format_line_stat("Bias (alt - ref)", "bias")
    format_line_stat("Interindividual variation (SD)", "sd.id")
    format_line_stat("Intraindividual variation (SD)", "sd.residual")
    format_line_stat("Total variation (SD)", "sd.combined")
    cat("Intraclass correlation\n")
    format_line_stat("  Intra/Total variance", "intraclass_correlation")
    cat("\n")
    cat("Limits of Agreement (95%)\n")
    format_line_stat("├ Upper limit", "loa.upr")
    format_line_stat("└ Lower limit", "loa.lwr")
    cat("\n")
    format_line_stat("Mean error (95%)", "mean.error.95")
    cat("\n")
    cat("--- Trending ---\n")
    format_line_stat("Intraindividual Mean error (95%)", "mean.error.individual.95")
    cat("\n")
    cat("Limits of Agreement for trending (95%)\n")
    format_line("├ Upper limit",
                ba_obj$BA_stats[["loa.trending"]],
                ba_obj$BA_stats_ci[["loa.trending"]][1],
                ba_obj$BA_stats_ci[["loa.trending"]][2]
                )
    format_line("└ Lower limit",
        -1*ba_obj$BA_stats[["loa.trending"]],
        -1*ba_obj$BA_stats_ci[["loa.trending"]][2],
        -1*ba_obj$BA_stats_ci[["loa.trending"]][1]
    )

    invisible(ba_obj)
}
