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
    format_line <- function(label, var) {
        cat(format(label, width = 29), ": ",
            sprintf("% 2.3f", ba_obj$BA_stats[[var]]),
            sprintf("[% 2.3f; % 2.3f]", ba_obj$BA_stats_ci[[var]][1], ba_obj$BA_stats_ci[[var]][2]), "\n")
    }

    cat(format("", width = 30), "    est", CI_label, "\n")
    format_line("Bias (alt - ref)", "bias")
    format_line("Interindividual variance (SD)", "sd.id")
    format_line("Intraindividual variance (SD)", "sd.residual")
    format_line("Total variance (SD)", "sd.combined")
    cat("\n")
    cat("Limits of Agreement (95%)\n")
    format_line("├ Upper limit", "loa.upr")
    format_line("└ Lower limit", "loa.lwr")

    invisible(ba_obj)
}
