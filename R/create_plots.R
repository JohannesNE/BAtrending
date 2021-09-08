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
plot_BA <- function(ba_obj, subject_legend = FALSE, normalize_log_loa = FALSE) {
    stopifnot("ba_analysis" %in% class(ba_obj))

    ba_obj_name <- deparse(substitute(ba_obj))

    if (normalize_log_loa) {
        stopifnot(attr(ba_obj, "logtrans"))

        d <- ba_obj$.non_log_data
        name_var_ref <- ba_obj$.raw_var_names$ref_col
        name_var_alt <- ba_obj$.raw_var_names$alt_col
    } else {
        d <- ba_obj$data
        name_var_ref <- ba_obj$.var_names$ref_col
        name_var_alt <- ba_obj$.var_names$alt_col
    }

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]

    # Create geoms for BA estimates
    est_lines <- if (normalize_log_loa) {
        list(
            ggplot2::geom_abline(aes(slope = log_estimate_to_mean_difference_slope(est), intercept = 0),
                                 linetype = 2, data = BA_stats)
        )
        }
    else{
        list(
        ggplot2::geom_hline(aes(yintercept = est),
                            linetype = 2,
                            data = BA_stats),

        ggplot2::geom_text(aes(x = Inf, y = est, label = label),
                           hjust = "inward",
                           vjust = -0.5,
                           data = BA_stats, inherit.aes = FALSE)
        )
    }

    # Add list of geoms for CIs
    if (is.null(ba_obj$BA_stats_ci)) {
        ci_shade <- NULL
        message(sprintf("The BA analysis object (%1$s) has no confidence intervals.
To add confidence intervals use `%1$s <- add_confint(%1$s)` (see ?add_confint)",
        ba_obj_name
        ))
    }
    else {

        ci_shade <- if (normalize_log_loa) {
                # Create data from sloped ribbons
                ci_shade_df <- merge(BA_stats, tibble(x = c(min(d$mean)*0.95, max(d$mean)*1.05)))
                ci_shade_df <- dplyr::mutate(ci_shade_df,
                                             ci.lwr = x * log_estimate_to_mean_difference_slope(ci.lwr),
                                             ci.upr = x * log_estimate_to_mean_difference_slope(ci.upr))

                ggplot2::geom_ribbon(aes(x = x, ymin = ci.lwr, ymax = ci.upr, group = stat),
                                   alpha = 0.5, fill = "gray",
                                   data = ci_shade_df,
                                   inherit.aes = FALSE)

            } else {
                ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = ci.lwr, ymax = ci.upr),
                                  alpha = 0.5, fill = "gray",
                                  data = BA_stats,
                                  inherit.aes = FALSE)
            }
    }

    BA_plot <- ggplot2::ggplot(d, aes(mean, diff, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::geom_hline(yintercept = 0, color = "gray") +
        ci_shade +
        est_lines +
        ggplot2::geom_point(show.legend = subject_legend) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.15))) +
        labs(title = "Bland Altman plot",
             x = glue::glue("Mean
                            ({name_var_ref} + {name_var_alt}) / 2"),
             y = glue::glue("Difference
                            {name_var_alt} - {name_var_ref}")) +
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


# Helper functions
log_estimate_to_mean_difference_slope <- function(est) {
    # formula from https://doi.org/10.1016/j.jclinepi.2007.11.003
    2*(exp(est)-1)/(exp(est) + 1)
}
