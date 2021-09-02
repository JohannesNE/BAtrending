#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#'
#' @importFrom ggplot2 aes
#' @export
#'
plot_BA <- function(ba_obj, subject_legend = FALSE) {
    stopifnot("ba_analysis" %in% class(ba_obj))

    ba_obj_name <- deparse(substitute(ba_obj))

    d <- ba_obj$data

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]

    # Add list of geoms for est
    est_lines <- list(
        ggplot2::geom_hline(aes(yintercept = est),
                            linetype = 2,
                            data = BA_stats),

        ggplot2::geom_text(aes(x = Inf, y = est, label = label),
                           hjust = "inward",
                           vjust = -0.5,
                           data = BA_stats, inherit.aes = FALSE)
        )

    # Add list of geoms for CIs
    if (is.null(ba_obj$BA_stats_ci)) {
        ci_shade <- NULL
        message(sprintf("The BA analysis object (%1$s) has no confidence intervals.
To add confidence intervals use `%1$s <- add_confint(%1$s)` (see ?add_confint)",
        ba_obj_name
        ))
    }
    else {
        ci_shade <- ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = ci.lwr, ymax = ci.upr),
                                  alpha = 0.5, fill = "gray",
                                  data = BA_stats,
                                  inherit.aes = FALSE)
    }

    BA_plot <- ggplot2::ggplot(d, aes(mean, diff, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::geom_hline(yintercept = 0, color = "gray") +
        ci_shade +
        est_lines +
        ggplot2::geom_point(show.legend = subject_legend) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.15))) +
        labs(title = "Bland Altman plot",
             x = glue::glue("Mean
                            ({ba_obj$.var_names$ref_col} + {ba_obj$.var_names$alt_col}) / 2"),
             y = glue::glue("Difference
                            {ba_obj$.var_names$alt_col} - {ba_obj$.var_names$ref_col}")) +
        theme_ba()

    BA_plot
}

# Helper functions for plots
breaks_from_vec <- function(x, vec = c(0.33, 0.5, 1, 2, 3),
                            fallback_fun = scales::breaks_log(base = 1.5, n = 5)) {
    # Selects breaks from a predefined vector of desired breaks. If less than
    # 3 breaks are in range, use fallback function
    res <- vec[vec < max(x) & vec > min(x)]
    if (length(res) < 3) {
        fallback_fun(x)
    } else {
        res
    }
}


#' Exponentiate Y scale
#'
#' @param labels
#' @export
label_y_exp_percent <- function(labels = scales::percent, ...) {
    list(ggplot2::scale_y_continuous(trans = "exp", labels = labels, ...),
         ggplot2::coord_trans(y = "log")
         )

}


#' Add secondary exponential Y scale
#'
#' @param labels
#' @export
scale_y2_exp_percent <- function(labels = scales::percent, ...) {
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(trans = exp, labels = labels,
                                                           breaks = breaks_from_vec, ...))
}

#' Exponentiate X scale
#'
#' @param labels
#' @export
label_x_exp_percent <- function(labels = scales::percent, ...) {
    list(ggplot2::scale_x_continuous(trans = "exp", labels = labels, ...),
         ggplot2::coord_trans(x = "log")
    )
}

#' Add secondary exponential X scale
#'
#' @param labels
#' @export
scale_x2_exp_percent <- function(labels = scales::percent, ...) {
    ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(trans = exp, labels = labels,
                                                           breaks = breaks_from_vec, ...))
}


