#' Print method for ba_analysis objects
#'
#' @param ba_obj
#'
#' @export
print.ba_analysis <- function(ba_obj) {
    ops <- options(digits = 3)
    on.exit(options(ops))

    n_obs <- ba_obj$diff_model@devcomp$dims[["n"]]
    n_sub <- nlevels(ba_obj$diff_model@flist[[1]])

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
        cat(format(label, width = 30), ": ",
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
    format_line_stat("Between subject variation (SD)", "sd.between")
    format_line_stat("Within subject variation (SD)", "sd.within")
    format_line_stat("Total variation (SD)", "sd.total")
    cat("Intraclass correlation\n")
    format_line_stat("  Between/Total variance", "intraclass.correlation")
    cat("\n")
    cat("Limits of agreement (95%)\n")
    format_line_stat("\u251C Upper limit", "loa.upr")
    format_line_stat("\u2514 Lower limit", "loa.lwr")
    cat("\n")
    format_line_stat("Perc. error (95%)", "percentage.error")
    cat("\n")
    cat("--- Trending ---\n")
    format_line_stat("Trending precicion (95%)", "trending.precision")
    format_line_stat("Change LoA [±] (95%)", "change.loa")
    
    invisible(ba_obj)
}
