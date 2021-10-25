#' Create Bland Altman Plot
#'
#' @param ba_obj BA analysis object
#' @param show_subject_legend Show legend for subjects
#' @param normalize_log_loa Plot BA estimates from log transformed data on raw data.
#' @param exponentiate Exponentiate values and parameters before plotting
#' @param use_non_log_x_values Plot using `plot_normalized_log_BA()`
#'
#' @return
#'
#' @examples
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' @export
plot_BA <- function(ba_obj, show_subject_legend = FALSE,
                    normalize_log_loa = FALSE,
                    exponentiate = FALSE,
                    use_non_log_x_values = TRUE) {
    assert_BA_obj(ba_obj)

    if (normalize_log_loa) return(plot_normalized_log_BA(ba_obj, show_subject_legend = show_subject_legend))
    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (exponentiate && !data_is_log_transformed) warning("Data was not log transformed by `compare_methods()`\nResults may be nonsesical")

    ba_obj_name <- deparse(substitute(ba_obj))

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]

    d <- ba_obj$data
    name_var_ref <- ba_obj$.var_names$ref_col
    name_var_alt <- ba_obj$.var_names$alt_col
    raw_name_var_ref <- ba_obj$.raw_var_names$ref_col
    raw_name_var_alt <- ba_obj$.raw_var_names$alt_col

    # Create axis names
    x_name <- if(use_non_log_x_values) {
        glue::glue("Mean
                   ({raw_name_var_ref} + {raw_name_var_alt}) / 2")
    } else {
        glue::glue("Mean
                   ({name_var_ref} + {name_var_alt}) / 2")
    }
    y_name <- if(exponentiate) {
            glue::glue("Ratio
                        {raw_name_var_alt} / {raw_name_var_ref}")
        } else {
            glue::glue("Difference
                        {name_var_alt} - {name_var_ref}")
    }

    if(use_non_log_x_values) d$mean <- ba_obj$.non_log_data$mean

    if(exponentiate) {
        d$diff <- exp(d$diff)
        BA_stats$est <- exp(BA_stats$est)
        BA_stats$ci.upr <- exp(BA_stats$ci.upr)
        BA_stats$ci.lwr <- exp(BA_stats$ci.lwr)

        null_value <- 1
        y_scale = list(ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                                                   breaks = breaks_from_vec),
                       ggplot2::coord_trans(y = "log"))
    } else {
        null_value <- 0
        y_scale = NULL
    }

    BA_stats <- dplyr::mutate(
        BA_stats,
        label_w_val = if (exponentiate) {

            sprintf(
                "%s\n(%s = %.2f \u00D7 %s)",
                .data$label,
                raw_name_var_alt,
                .data$est,
                raw_name_var_ref
            )
        } else {
            sprintf("%s (%+.2f)",
                    .data$label,
                    .data$est)
        }
    )


    # Create geoms for BA estimates
    est_lines <- list(
        ggplot2::geom_hline(aes(yintercept = .data$est),
                            linetype = 2,
                            data = BA_stats),

        ggplot2::geom_label(aes(x = Inf, y = .data$est, label = .data$label_w_val),
                           hjust = "inward", vjust = 0,
                           fill = NA, label.size = NA,
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

        ci_shade <- ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .data$ci.lwr, ymax = .data$ci.upr),
                                  alpha = 0.5, fill = "gray",
                                  data = BA_stats,
                                  inherit.aes = FALSE)

    }



    ggplot2::ggplot(d, aes(mean, diff, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.20))) +
        ggplot2::geom_hline(yintercept = null_value, color = "gray") +
        ci_shade +
        est_lines +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        ggplot2::labs(x = x_name,
             y = y_name) +
        y_scale +
        theme_ba()


}

#' Plot BA estimates from log transformed data on raw data.
#'
#' @param ba_obj
#' @param show_subject_legend
#'
#' @return
#' @export
#'
#' @examples
plot_normalized_log_BA <- function(ba_obj, show_subject_legend = FALSE) {
    assert_BA_obj(ba_obj)
    ba_obj_name <- deparse(substitute(ba_obj))

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if(!data_is_log_transformed) stop("Plotting normalized log LoA is only valid for comparisons on the log-scale")

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]


    # Use non-log data and names for plotting
    d <- ba_obj$.non_log_data
    name_var_ref <- ba_obj$.raw_var_names$ref_col
    name_var_alt <- ba_obj$.raw_var_names$alt_col

    # Create labels
    BA_stats <- dplyr::mutate(BA_stats,
        label_w_val = sprintf("%s\n(%s = %.2f \u00D7 %s)", # unicode times
                              .data$label,
                              name_var_alt,
                              exp(.data$est),
                              name_var_ref
                              ),
        slope = log_estimate_to_mean_difference_slope(.data$est))

    est_lines <- list(
            ggplot2::geom_abline(aes(slope = .data$slope, intercept = 0),
                                 linetype = 2, data = BA_stats),

            ggplot2::geom_text(aes(x = set_limits(d$mean)[2],
                                   y = set_limits(d$mean)[2] * .data$slope,
                                   label = .data$label_w_val),
                               hjust = -0.05,
                               data = BA_stats, inherit.aes = FALSE),
            ggplot2::coord_cartesian(xlim = set_limits(d$mean), clip = "off")

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
        # Create data from sloped ribbons
        ci_shade_df <- merge(BA_stats, data.frame(x = c(min(d$mean)*0.95, max(d$mean)*1.02)))

        ci_shade_df <- dplyr::mutate(ci_shade_df,
            ci.lwr = .data$x * log_estimate_to_mean_difference_slope(.data$ci.lwr),
            ci.upr = .data$x * log_estimate_to_mean_difference_slope(.data$ci.upr))

        ci_shade <- ggplot2::geom_ribbon(aes(x = .data$x, ymin = .data$ci.lwr, ymax = .data$ci.upr, group = .data$stat),
                             alpha = 0.5, fill = "gray",
                             data = ci_shade_df,
                             inherit.aes = FALSE)


    }

    ggplot2::ggplot(d, aes(.data$mean, .data$diff, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
        ggplot2::geom_hline(yintercept = 0, color = "gray") +
        ci_shade +
        est_lines +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        ggplot2::labs(x = glue::glue("Mean
                            ({name_var_ref} + {name_var_alt}) / 2"),
                      y = glue::glue("Difference
                            {name_var_alt} - {name_var_ref}")) +
        theme_ba() +
        ggplot2::theme(plot.margin = ggplot2::margin(1, 8, 1, 1, unit = "lines"))



}

# Helper functions for plots
breaks_from_vec <- function(x, vec = c(0.33, 0.5, 0.66, 1, 1.5, 2, 3),
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

# Helper functions
log_estimate_to_mean_difference_slope <- function(est) {
    # formula from https://doi.org/10.1016/j.jclinepi.2007.11.003
    2*(exp(est)-1)/(exp(est) + 1)
}

set_limits <- function(vec, rel_exp = 0.05, abs_exp = 0) {
    range_vec <- range(vec)
    expansion <- diff(range_vec) * rel_exp + abs_exp
    c(
        range_vec[1] - expansion,
        range_vec[2] + expansion
    )
}
