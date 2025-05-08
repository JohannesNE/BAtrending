#' Create Bland Altman Plot
#'
#' @param ba_obj BA analysis object
#' @param show_subject_legend Show legend for subjects
#' @param keep_log_scale Show log transformed differences. If `FALSE` (default), values and parameters are exponentiated before plotting
#'
#' @returns Bland Altman plot (ggplot)
#'
#' @examples plot_BA(compare_methods(CO, "ic", "rv", id_col = "sub"))
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' @export
plot_BA <- function(ba_obj, show_subject_legend = FALSE,
                    normalize_log_loa = FALSE,
                    keep_log_scale = FALSE) {
    assert_BA_obj(ba_obj)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) stop("Data was not log transformed by `compare_methods()`")

    ba_obj_name <- deparse(substitute(ba_obj))
    check_CI(ba_obj, ba_obj_name)

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]


    d <- ba_obj$data
    d$mean <- ba_obj$.non_log_data$mean
    
    var_names <- ba_obj$.var_names
    var_names_raw <- ba_obj$.var_names_raw

    if(data_is_log_transformed && !keep_log_scale) {
        exponentiated <- TRUE
        d$diff <- exp(d$diff)
        BA_stats$est <- exp(BA_stats$est)
        BA_stats$ci.upr <- exp(BA_stats$ci.upr)
        BA_stats$ci.lwr <- exp(BA_stats$ci.lwr)

        null_value <- 1
        y_scale = list(ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                                                   breaks = breaks_from_vec),
                       ggplot2::coord_trans(y = "log", clip = "off"))
    } else {
        exponentiated <- FALSE
        null_value <- 0
        y_scale = NULL
    }

    ggplot2::ggplot(d, aes(mean, diff, color = .data[[var_names$id_col]])) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.20))) +
        ggplot2::geom_hline(yintercept = null_value, color = "gray") +
        add_BA_stats_geom(BA_stats, exponentiated = exponentiated, name_ref = var_names_raw$ref_col, name_alt = var_names_raw$alt_col) +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated) +
        y_scale +
        theme_ba()


}

#' @importFrom ggplot2 aes
#' @importFrom rlang .data
add_BA_stats_geom <- function(BA_stats_df, exponentiated = FALSE, name_ref = "ref", name_alt = "alt") {
    BA_stats_df <- dplyr::mutate(
        BA_stats_df,
        label_w_val = if (exponentiated) {

            sprintf(
                "%s\n(%s = %.2f \u00D7 %s)",
                .data$label,
                name_alt,
                .data$est,
                name_ref
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
                            data = BA_stats_df),

        ggplot2::geom_label(aes(x = Inf, y = .data$est, label = .data$label_w_val),
                           hjust = "inward", vjust = 0,
                           fill = NA, label.size = NA,
                           data = BA_stats_df, inherit.aes = FALSE)
        )


    # Add list of geoms for CIs
    if (is.na(BA_stats_df$ci.lwr[1])) {
        ci_shade <- NULL
    }
    else {

        ci_shade <- ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .data$ci.lwr, ymax = .data$ci.upr),
                                  alpha = 0.5, fill = "gray",
                                  data = BA_stats_df,
                                  inherit.aes = FALSE)

    }

    list(ci_shade, est_lines)
}

#' Manually add Bland Altman geometry to plot  
#' 
#' @param bias,loa.lwr,loa.upr estimates to be plotted. Optionally including confidence intervals
#' as `c(est, ci.lwr, ci.upr)`.
#' @param exponentiated Set true if estimates are exponentiated estimates from a model on log-transformed data.
#' Treats estimates as ratios.
#' @param name_ref,name_alt Name of reference and alternative method. Only used if `exponentiated = TRUE`
#' 
#' @export
add_BA_stats_geom_manual <- function(bias, loa.lwr, loa.upr, 
    exponentiated = FALSE,
    name_ref = "ref",
    name_alt = "alt") {
    stopifnot(
        length(bias) == length(loa.lwr),
        length(bias) == length(loa.upr)
    )
    
    # Pad NA if there is no CI
    if (length(bias) == 1) {
        bias[2:3] <- NA
        loa.lwr[2:3] <- NA
        loa.upr[2:3] <- NA
    }

    BA_stats <- as.data.frame(rbind(bias, loa.lwr, loa.upr))
    names(BA_stats) <- c("est", "ci.lwr", "ci.upr")
    BA_stats$label <- c("Bias", "95% LoA", "95% LoA")

    add_BA_stats_geom(BA_stats, exponentiated = exponentiated, name_alt = name_alt, name_ref = name_ref)
}

#' Plot BA estimates from log transformed data on raw data.
#'
#' @inheritParams plot_BA
#'
#' @return Bland Altman style plot with relative differences plotted on absolute differences.
#'
#' @examples 
#' plot_BA(compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE))
#' 
#' @export
plot_BA_normalized_log <- function(ba_obj, show_subject_legend = FALSE) {
    assert_BA_obj(ba_obj)
    ba_obj_name <- deparse(substitute(ba_obj))
    check_CI(ba_obj, ba_obj_name)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if(!data_is_log_transformed) stop("Plotting normalized log LoA is only valid for comparisons on the log-scale")

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]


    # Use non-log data and names for plotting
    d <- ba_obj$.non_log_data
    var_names <- ba_obj$.var_names_raw

    # Create labels
    BA_stats <- dplyr::mutate(BA_stats,
        label_w_val = sprintf("%s\n(%s = %.2f \u00D7 %s)", # unicode times
                              .data$label,
                              var_names$alt_col,
                              exp(.data$est),
                              var_names$ref_col
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
                            ({var_names$ref_col} + {var_names$alt_col}) / 2"),
                      y = glue::glue("Difference
                            {var_names$alt_col} - {var_names$ref_col}")) +
        theme_ba() +
        ggplot2::theme(plot.margin = ggplot2::margin(1, 8, 1, 1, unit = "lines"))



}


#' Plot intraindividual variation (model residuals) in differences and means.
#'
#' @inheritParams plot_BA
#' 
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' 
#' @export
plot_BA_residuals <- function(ba_obj, show_subject_legend = FALSE,
    keep_log_scale = FALSE) {
    
    assert_BA_obj(ba_obj)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) stop("Data was not log transformed by `compare_methods()`")

    diff_residuals <- residuals(ba_obj$diff_model)
    mean_residuals <- residuals(ba_obj$mean_model)

    # Y axis
    if(data_is_log_transformed && !keep_log_scale) {
        exponentiated <- TRUE
        diff_residuals <- exp(diff_residuals)

        null_value <- 1
        y_scale = list(ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                                                   breaks = breaks_from_vec),
                       ggplot2::coord_trans(y = "log"))
    } else {
        exponentiated <- FALSE
        null_value <- 0
        y_scale = NULL
    }

    d <- ba_obj$data
    d$mean_residuals <- mean_residuals
    d$diff_residuals <- diff_residuals

    ggplot2::ggplot(d, aes(mean_residuals, diff_residuals, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::geom_hline(yintercept = null_value, color = "gray") +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        y_scale +
        create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated) +
        theme_ba()
}

#' Make scatter plot of paired measurements in analysis.
#'
#' @inheritParams plot_BA
#' 
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' 
#' @export
plot_BA_scatter <- function(ba_obj, show_subject_legend = FALSE,
    keep_log_scale = FALSE) {
    assert_BA_obj(ba_obj)
    
    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) stop("Data was not log transformed by `compare_methods()`")
    
    var_names <- ba_obj$.var_names
    var_names_raw <- ba_obj$.var_names_raw
    
    if (keep_log_scale) {
        d <- ba_obj$data
        label_names <- var_names
    } else {
        d <- ba_obj$.non_log_data
        label_names <- var_names_raw
    }

    ggplot2::ggplot(d, aes(
            .data[[var_names_raw$ref_col]], 
            .data[[var_names_raw$alt_col]], 
            color = .data[[var_names$id_col]])) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray") +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        ggplot2::labs(x=label_names$ref_col, y=label_names$alt_col) +
        theme_ba()
}

#' Plot all plots in extended Bland Altman analysis.
#' 
#' Creates a scatter plot, a standard Bland Altman plot and a residuals plot for assessing trending ability.
#'
#' @inheritParams plot_BA
#' @param equal_scales Plot the residuals on a plane with the scale of the original data.
#' 
#' @export
plot_BA_combine <- function(
    ba_obj,
    show_subject_legend = FALSE,
    equal_scales = TRUE,
    keep_log_scale = FALSE
    ) {
    assert_BA_obj(ba_obj)

    # Create scatter plot
    scatter_plot <- plot_BA_scatter(ba_obj, 
        show_subject_legend = show_subject_legend,
        keep_log_scale = keep_log_scale) 
    
    # Create Bland Altman plot
    BA_plot <- plot_BA(ba_obj,
        show_subject_legend = show_subject_legend,
        keep_log_scale = keep_log_scale)
    
    # Create residuals plot
    residuals_plot <- plot_BA_residuals(ba_obj, 
        show_subject_legend = show_subject_legend,
        keep_log_scale = keep_log_scale)
    
    if (equal_scales) {
        ratio_scale <- attr(ba_obj, "logtrans") && !keep_log_scale

        # Find limits of BA_plot
        BA_plot_build <- ggplot2::ggplot_build(BA_plot)
        
        # Data Ranges
        # y_data_range <- BA_plot_build$layout$panel_scales_y[[1]]$range$range 
        # x_data_range <- BA_plot_build$layout$panel_scales_x[[1]]$range$range 

        # Panel limits
        y_panel_limits <- BA_plot_build$layout$panel_params[[1]]$y.range
        x_panel_limits <- BA_plot_build$layout$panel_params[[1]]$x.range
        
        if(ratio_scale) {
            y_scale <- ggplot2::scale_y_continuous(
                limits = exp(y_panel_limits), 
                expand = c(0,0),
                labels = scales::label_number(accuracy = 0.01),
                breaks = breaks_from_vec
            )
        } else {
            y_scale <- ggplot2::scale_y_continuous(limits = y_panel_limits, expand = c(0,0))
        }

        # Set scales for residual plot
        residuals_plot <- residuals_plot + 
            ggplot2::scale_x_continuous(limits = x_panel_limits - mean(x_panel_limits, expand = c(0,0))) + 
            y_scale

        }
    
    # Combine plots
    patchwork::wrap_plots(scatter_plot, BA_plot, residuals_plot, guides = "collect")
    
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

create_axis_labels <- function(ba_obj, exponentiated = FALSE) {
    name_var_ref <- ba_obj$.var_names$ref_col
    name_var_alt <- ba_obj$.var_names$alt_col
    raw_name_var_ref <- ba_obj$.var_names_raw$ref_col
    raw_name_var_alt <- ba_obj$.var_names_raw$alt_col

    # Create axis names
    x_name <- glue::glue("Mean
                   ({raw_name_var_ref} + {raw_name_var_alt}) / 2")
    
    y_name <- if(exponentiated) {
            glue::glue("Ratio
                        {raw_name_var_alt} / {raw_name_var_ref}")
        } else {
            glue::glue("Difference
                        {name_var_alt} - {name_var_ref}")
    }

    ggplot2::labs(x = x_name, y = y_name)
}

check_CI <- function(ba_obj, ba_obj_name = "ba_obj") {
    # Check for CI
    if (is.null(ba_obj$BA_stats_ci)) {
        message(sprintf("The BA analysis object has no confidence intervals.
To add confidence intervals use `%1$s <- add_confint(%1$s)` (see ?add_confint)",
        ba_obj_name
        ))
    }
}