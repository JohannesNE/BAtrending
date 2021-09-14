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
plot_BA <- function(ba_obj, subject_legend = FALSE,
                    normalize_log_loa = FALSE) {
    if(!("ba_analysis" %in% class(ba_obj))) stop('`ba_obj` must be a "ba_analysis" object (from `compare_methods()`)')

    ba_obj_name <- deparse(substitute(ba_obj))

    data_is_log_transformed <- attr(ba_obj, "logtrans")

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]

    if (normalize_log_loa) {
        if(!data_is_log_transformed) stop("`normalize_log_loa = TRUE` is only valid for comparisons on the log-scale")

        d <- ba_obj$.non_log_data
        name_var_ref <- ba_obj$.raw_var_names$ref_col
        name_var_alt <- ba_obj$.raw_var_names$alt_col

        # Create labels
        BA_stats <- transform(BA_stats,
                              label_w_val = sprintf("%s\n(%s = %s × %.2f)",
                                                    label, name_var_alt, name_var_ref, exp(est)))

    } else {
        d <- ba_obj$data
        name_var_ref <- ba_obj$.var_names$ref_col
        name_var_alt <- ba_obj$.var_names$alt_col

        BA_stats <- transform(BA_stats,
                              label_w_val = sprintf("%s (%+.2f)", label, est))
    }

    # Create geoms for BA estimates
    est_lines <- if (normalize_log_loa) {
        BA_stats <- transform(BA_stats, slope = log_estimate_to_mean_difference_slope(est))

        list(
            ggplot2::geom_abline(aes(slope = slope, intercept = 0),
                                 linetype = 2, data = BA_stats),

            ggplot2::geom_text(aes(x = max(d$mean) * 1.03,
                                   y = max(d$mean) * 1.03 * slope,
                                   label = label_w_val),
                               hjust = -0.05,
                               data = BA_stats, inherit.aes = FALSE),
            ggplot2::coord_cartesian(xlim = c(min(d$mean) * 0.95, max(d$mean)), clip = "off")


        )
        }
    else{
        list(
        ggplot2::geom_hline(aes(yintercept = est),
                            linetype = 2,
                            data = BA_stats),

        ggplot2::geom_text(aes(x = max(d$mean) * 1.03, y = est, label = label_w_val),
                           hjust = -0.05,
                           data = BA_stats, inherit.aes = FALSE),
        ggplot2::coord_cartesian(xlim = c(min(d$mean) * 0.95, max(d$mean)), clip = "off")
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
                ci_shade_df <- merge(BA_stats, data.frame(x = c(min(d$mean)*0.95, max(d$mean)*1.02)))

                ci_shade_df <- transform(ci_shade_df,
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

    extra_margin <-  ggplot2::theme(plot.margin = ggplot2::margin(1, 8, 1, 1, unit = "lines"))

    BA_plot <- ggplot2::ggplot(d, aes(mean, diff, color = .data[[ba_obj$.var_names$id_col]])) +
        #ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.15))) +
        ggplot2::geom_hline(yintercept = 0, color = "gray") +
        ci_shade +
        est_lines +
        ggplot2::geom_point(show.legend = subject_legend) +
        ggplot2::labs(title = "Bland Altman plot",
             x = glue::glue("Mean
                            ({name_var_ref} + {name_var_alt}) / 2"),
             y = glue::glue("Difference
                            {name_var_alt} - {name_var_ref}")) +
        theme_ba() +
        extra_margin

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
