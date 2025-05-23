#' Create Bland Altman Plot
#'
#' @param ba_obj BA analysis object
#' @param fix_aspect_ratio Use aspect ratio of 1 between X and Y axis (sets `coord_fixed()`)
#' @param show_subject_legend Show legend for subjects
#' @param keep_log_scale Show log transformed differences. If `FALSE` (default), values and parameters are exponentiated before plotting
#'
#' @returns Bland Altman plot (ggplot)
#'
#' @examples 
#' ba_obj <- compare_methods(CO, ic, rv, id_col = sub)
#' plot_BA(ba_obj)
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' @export
plot_BA <- function(ba_obj, 
                    fix_aspect_ratio = FALSE,
                    show_subject_legend = FALSE,
                    keep_log_scale = FALSE) {
    assert_BA_obj(ba_obj)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) cli::cli_abort("Data was not log transformed by {.fn compare_methods()}.")
    if (fix_aspect_ratio && data_is_log_transformed) cli::cli_warn("Cant fix aspect ratio on log transformed data")

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
        y_scale_and_coord = list(ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                                                   breaks = breaks_from_vec),
                       ggplot2::coord_trans(y = "log", clip = "off"))
    } else {
        exponentiated <- FALSE
        null_value <- 0
        if(fix_aspect_ratio) {
            y_scale_and_coord <- ggplot2::coord_fixed(clip = "off")
        } else {
            y_scale_and_coord <- ggplot2::coord_cartesian(clip = "off")
        }

    }

    ggplot2::ggplot(d, aes(mean, diff, color = .data[[var_names$id_col]])) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.20))) +
        ggplot2::geom_hline(yintercept = null_value, color = "gray") +
        add_BA_stats_geom(BA_stats, exponentiated = exponentiated, name_ref = var_names_raw$ref_col, name_alt = var_names_raw$alt_col) +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated) +
        y_scale_and_coord +
        theme_ba()


}

#' @importFrom ggplot2 aes
#' @importFrom rlang .data
add_BA_stats_geom <- function(BA_stats_df, exponentiated = FALSE, name_ref = "ref", name_alt = "alt") {
    
    BA_stats_df$label_w_val <- if (exponentiated) {
            sprintf(
                "%s\n(%s = %.2f \u00D7 %s)",
                BA_stats_df$label,
                name_alt,
                BA_stats_df$est,
                name_ref
            )
        } else {
            sprintf("%s (%+.2f)",
                    BA_stats_df$label,
                    BA_stats_df$est)
        }

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
#' @param bias,lwr,upr estimates to be plotted. Optionally including confidence intervals
#' as `c(est, ci.lwr, ci.upr)`.
#' @param exponentiated Set true if estimates are exponentiated estimates from a model on log-transformed data.
#' Treats estimates as ratios.
#' @param name_ref,name_alt Name of reference and alternative method. Only used if `exponentiated = TRUE`
#' @param line_labels Labels for Bias and LoA lines. Must be a named vector with names: 'bias', 'lwr', 'upr'
#' 
#' @export
add_BA_stats_geom_manual <- function(bias, lwr, upr,
    exponentiated = FALSE,
    name_ref = "ref",
    name_alt = "alt",
    line_labels = c(bias = "Bias", lwr = "95% LoA", upr = "95% LoA")) {
    
    BA_stats <- as.data.frame(rbind(bias = bias[1:3], lwr = lwr[1:3], upr = upr[1:3]))
    names(BA_stats) <- c("est", "ci.lwr", "ci.upr")

    if (!all(c("bias", "lwr", "upr") %in% names(line_labels))) {
        cli::cli_abort("{.arg line_labels} must be a named vector with names: 'bias', 'lwr', 'upr'")
    }
    BA_stats$label <- line_labels[rownames(BA_stats)]

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
plot_BA_normalized_log <- function(
    ba_obj, 
    show_subject_legend = FALSE,
    fix_aspect_ratio = FALSE
) {
    assert_BA_obj(ba_obj)
    ba_obj_name <- deparse(substitute(ba_obj))
    check_CI(ba_obj, ba_obj_name)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if(!data_is_log_transformed) cli::cli_abort("Plotting normalized log LoA is only valid for comparisons on the log-scale")

    # Generate data frame with BA statistics
    BA_stats <- gen_ba_stats_df(ba_obj)
    BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"),]


    # Use non-log data and names for plotting
    d <- ba_obj$.non_log_data
    var_names <- ba_obj$.var_names_raw

    # Create labels
    BA_stats$label_w_val <- sprintf(
        "%s\n(%s = %.2f \u00D7 %s)", # unicode times
        BA_stats$label,
        var_names$alt_col,
        exp(BA_stats$est),
        var_names$ref_col
    )

    BA_stats$slope <- log_estimate_to_mean_difference_slope(BA_stats$est)

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

        ci_shade_df$ci.lwr <- ci_shade_df$x * log_estimate_to_mean_difference_slope(ci_shade_df$ci.lwr)
        ci_shade_df$ci.upr <- ci_shade_df$x * log_estimate_to_mean_difference_slope(ci_shade_df$ci.upr)
        
        ci_shade <- ggplot2::geom_ribbon(aes(x = .data$x, ymin = .data$ci.lwr, ymax = .data$ci.upr, group = .data$stat),
                             alpha = 0.5, fill = "gray",
                             data = ci_shade_df,
                             inherit.aes = FALSE)


    }

    if(fix_aspect_ratio) {
        plot_coord <- ggplot2::coord_fixed(clip = "off")
    } else {
        plot_coord <- ggplot2::coord_cartesian(clip = "off")
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
        plot_coord + 
        theme_ba() +
        ggplot2::theme(plot.margin = ggplot2::margin(1, 8, 1, 1, unit = "lines"))



}


#' Plot intraindividual variation (model residuals) in differences and means.
#'
#' @inheritParams plot_BA
#' @param show_sd Mark 1.96*SD (intraindividual) on the plot (analouge to 95% LoA).
#' 
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' 
#' @export
plot_BA_residuals <- function(
    ba_obj, 
    fix_aspect_ratio = FALSE,
    show_subject_legend = FALSE,
    keep_log_scale = FALSE,
    show_sd = TRUE) {
    
    assert_BA_obj(ba_obj)

    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) cli::cli_abort("Data was not log transformed by {.fn compare_methods()}.")

    diff_residuals <- residuals(ba_obj$diff_model)
    mean_residuals <- residuals(ba_obj$mean_model)

    # Y axis
    if(data_is_log_transformed && !keep_log_scale) {
        exponentiated <- TRUE
        diff_residuals <- exp(diff_residuals)

        null_value <- 1
        y_scale_and_coord = list(ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01),
                                                   breaks = breaks_from_vec),
                       ggplot2::coord_trans(y = "log"))
    } else {
        exponentiated <- FALSE
        null_value <- 0
        if(fix_aspect_ratio) {
            y_scale_and_coord <- ggplot2::coord_fixed(clip = "off")
        } else {
            y_scale_and_coord <- ggplot2::coord_cartesian(clip = "off")
        }
    }

    if (show_sd) {
        sd.within <- c(ba_obj$BA_stats$sd.within, ba_obj$BA_stats_ci$sd.within)

        if (exponentiated) {
            upr <- exp(1.96 * sd.within)
            lwr <- exp(-1.96 * sd.within)
        } else{
            upr <- 1.96 * sd.within
            lwr <- -1.96 * sd.within
        }

        stats_geom <- list(add_BA_stats_geom_manual(bias = NULL, 
            lwr = lwr, 
            upr = upr,
            line_labels = c(bias = "", lwr = "-1.96SD", upr = "+1.96SD")),
            ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.20)))
        )
    } else {
        stats_geom <- NULL
    }

    d <- ba_obj$data
    d$mean_residuals <- mean_residuals
    d$diff_residuals <- diff_residuals

    residual_plot <- ggplot2::ggplot(d, aes(mean_residuals, diff_residuals, color = .data[[ba_obj$.var_names$id_col]])) +
        ggplot2::geom_hline(yintercept = null_value, color = "gray") +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        y_scale_and_coord +
        stats_geom + 
        create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated) +
        theme_ba()

    # Update x-axis label to "Residual mean"
    current_label_x <- residual_plot$labels$x
    residual_plot$labels$x <- sub("^M", "Residual m", current_label_x)

    # Update y-axis label to "Residual difference"
    current_label_y <- residual_plot$labels$y
    residual_plot$labels$y <- sub("^D", "Residual d", current_label_y)

    residual_plot
}

#' Make scatter plot of paired measurements in analysis.
#'
#' @inheritParams plot_BA
#' 
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' 
#' @export
plot_BA_scatter <- function(
    ba_obj, 
    fix_aspect_ratio = FALSE,
    show_subject_legend = FALSE,
    keep_log_scale = FALSE) {
    assert_BA_obj(ba_obj)
    
    data_is_log_transformed <- attr(ba_obj, "logtrans")
    if (keep_log_scale && !data_is_log_transformed) cli::cli_abort("Data was not log transformed by {.fn compare_methods()}.")
    
    var_names <- ba_obj$.var_names
    var_names_raw <- ba_obj$.var_names_raw
    
    if (keep_log_scale) {
        d <- ba_obj$data
        label_names <- var_names
    } else {
        d <- ba_obj$.non_log_data
        label_names <- var_names_raw
    }

    plot_coord <- if(fix_aspect_ratio) ggplot2::coord_fixed() else NULL

    ggplot2::ggplot(d, aes(
            .data[[var_names_raw$ref_col]], 
            .data[[var_names_raw$alt_col]], 
            color = .data[[var_names$id_col]])) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray") +
        ggplot2::geom_point(show.legend = show_subject_legend) +
        ggplot2::labs(x=label_names$ref_col, y=label_names$alt_col) +
        plot_coord +
        theme_ba()
}

#' Plot all plots in extended Bland Altman analysis.
#' 
#' Creates a scatter plot, a standard Bland Altman plot and a residuals plot for assessing trending ability.
#'
#' @inheritParams plot_BA
#' @param equal_scales Plot the residuals on a plane with the scale of the original data.
#' @param return_as_list Return the three plots in a list. If `FALSE`, the plots are combined using {patchwork}.
#' 
#' @export
plot_BA_combine <- function(
    ba_obj,
    fix_aspect_ratio = FALSE,
    show_subject_legend = FALSE,
    equal_scales = TRUE,
    keep_log_scale = FALSE,
    return_as_list = FALSE
    ) {
    assert_BA_obj(ba_obj)

    # Create scatter plot
    scatter_plot <- plot_BA_scatter(ba_obj, 
        fix_aspect_ratio,
        show_subject_legend = show_subject_legend,
        keep_log_scale = keep_log_scale) 
    
    # Create Bland Altman plot
    BA_plot <- plot_BA(ba_obj,
        show_subject_legend = show_subject_legend,
        fix_aspect_ratio,
        keep_log_scale = keep_log_scale)
    
    # Create residuals plot
    residuals_plot <- plot_BA_residuals(ba_obj, 
        fix_aspect_ratio,
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
    if (return_as_list) {
        list(scatter_plot = scatter_plot, BA_plot = BA_plot, residuals_plot = residuals_plot)
    } else {
        patchwork::wrap_plots(scatter_plot, BA_plot, residuals_plot, guides = "collect")
    }
    
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