#' Create Bland-Altman plot
#'
#' Creates a standard Bland-Altman plot from a Bland-Altman analysis object made with [compare_methods()].
#'
#' @param ba_obj Bland-Altman analysis object
#' @param aspect_ratio Set aspect ratio (x/y) between X and Y axis (sets `ggplot2::coord_fixed()`), Default (NULL) is automatic.
#' @param show_subject_legend Show legend for subjects
#' @param keep_log_scale Show log transformed differences. If `FALSE` (default), values and parameters are exponentiated before plotting
#'
#' @returns Bland-Altman plot (ggplot)
#'
#' @seealso [plot_BA_normalized_log()] which shows the results of a proportional Bland-Altman analysis (with log-transformed measurements)
#' on the non-transformed data.
#'
#' @examples
#' ba_obj <- compare_methods(CO, ic, rv, id_col = sub)
#' plot_BA(ba_obj)
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#' @export
plot_BA <- function(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE
) {
  assert_BA_obj(ba_obj)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (keep_log_scale && !data_is_log_transformed) {
    cli::cli_abort(
      "Data was not log transformed by {.fn compare_methods()}."
    )
  }

  ba_obj_name <- deparse(substitute(ba_obj))
  check_CI(ba_obj, ba_obj_name)

  # Generate data frame with BA statistics
  BA_stats <- gen_ba_stats_df(ba_obj)
  BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"), ]

  d <- ba_obj$data
  # always use the non-log values for the x-axis (avg)
  d$avg <- ba_obj$.non_log_data$avg

  var_names <- ba_obj$.var_names
  var_names_raw <- ba_obj$.var_names_raw

  # Exponentiate data if relevant
  if (data_is_log_transformed && !keep_log_scale) {
    exponentiated <- TRUE
    d$diff <- exp(d$diff)
    BA_stats$est <- exp(BA_stats$est)
    BA_stats$ci.upr <- exp(BA_stats$ci.upr)
    BA_stats$ci.lwr <- exp(BA_stats$ci.lwr)
  } else {
    exponentiated <- FALSE
  }

  # Prepare plot settings
  plot_setup <- BA_plot_setup(
    exponentiated = exponentiated,
    data_is_log_transformed = data_is_log_transformed,
    aspect_ratio = aspect_ratio
  )

  # To put the BA_stats geoms in correct order, first make the list of geoms, and then add them individually
  BA_stats_geoms <- add_BA_stats_geom(
    BA_stats,
    exponentiated = exponentiated,
    name_ref = var_names_raw$ref_col,
    name_alt = var_names_raw$alt_col
  )

  ggplot2::ggplot(d, aes(avg, diff, color = .data[[var_names$id_col]])) +
    plot_setup +
    BA_stats_geoms[c("rect", "hline")] +
    ggplot2::geom_point(show.legend = show_subject_legend) +
    BA_stats_geoms["label"] +
    create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated)
}

# Internal function to add ggplot settings to plot_BA and plot_BA_residuals
BA_plot_setup <- function(
  exponentiated,
  data_is_log_transformed,
  aspect_ratio
) {
  # Default values
  coord <- ggplot2::coord_cartesian(clip = "off")
  y_scale <- NULL
  null_value <- 0

  if (is.numeric(aspect_ratio)) {
    if (!data_is_log_transformed) {
      # Only set aspect ratio in non-exp plots
      coord <- ggplot2::coord_fixed(
        ratio = aspect_ratio,
        clip = "off"
      )
    } else {
      cli::cli_inform(c(
        i = "Aspect ratio is not applied to Bland-Altman plot of log transformed data as the axes are on different scales.",
        "X: absolute.",
        "Y: ratio or log-transformed."
      ))
    }
  }

  if (exponentiated) {
    null_value <- 1
    y_scale <- ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01),
      breaks = breaks_from_vec
    )
    coord <- ggplot2::coord_trans(y = "log", clip = "off")
  }

  hline <- ggplot2::geom_hline(yintercept = null_value, color = "gray")
  x_scale <- ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(0.1, 0.20))
  )

  list(
    hline = hline,
    y_scale = y_scale,
    x_scale = x_scale,
    coord = coord,
    theme = theme_ba()
  )
}

#' @importFrom ggplot2 aes
#' @importFrom rlang .data
add_BA_stats_geom <- function(
  BA_stats_df,
  exponentiated = FALSE,
  name_ref = "ref",
  name_alt = "alt"
) {
  BA_stats_df$label_w_val <- if (exponentiated) {
    sprintf(
      "%s\n(%s = %.2f \u00D7 %s)",
      BA_stats_df$label,
      name_alt,
      BA_stats_df$est,
      name_ref
    )
  } else {
    sprintf("%s (%+.2f)", BA_stats_df$label, BA_stats_df$est)
  }

  # If there is more than one stat label, place the lowest below the line to avoid overlapping
  label_vjust <- 0
  if (nrow(BA_stats_df) > 1) {
    label_vjust <- rep(0, nrow(BA_stats_df))
    lowest_i <- which.min(BA_stats_df$est)
    label_vjust[lowest_i] <- 1
  }

  # Create geoms for BA estimates
  est_lines <- list(
    "hline" = ggplot2::geom_hline(
      aes(yintercept = .data$est),
      linetype = 2,
      data = BA_stats_df
    ),

    "label" = ggplot2::geom_label(
      aes(x = Inf, y = .data$est, label = .data$label_w_val),
      hjust = "inward",
      vjust = label_vjust, # Place loa.lwr below line
      fill = NA,
      label.size = NA,
      data = BA_stats_df,
      inherit.aes = FALSE
    )
  )

  # Add list of geoms for CIs
  if (is.na(BA_stats_df$ci.lwr[1])) {
    ci_shade <- NULL
  } else {
    ci_shade <- ggplot2::geom_rect(
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data$ci.lwr,
        ymax = .data$ci.upr
      ),
      alpha = 0.5,
      fill = "gray",
      data = BA_stats_df,
      inherit.aes = FALSE
    )
  }

  c(list("rect" = ci_shade), est_lines)
}

#' Manually add Bland-Altman geometry to plot
#'
#' @param bias,lwr,upr estimates to be plotted. Optionally including confidence intervals
#' as `c(est, ci.lwr, ci.upr)`.
#' @param exponentiated Set true if estimates are exponentiated estimates from a model on log-transformed data.
#' Treats estimates as ratios.
#' @param name_ref,name_alt Name of reference and alternative method. Only used if `exponentiated = TRUE`
#' @param line_labels Labels for Bias and LoA lines. Must be a named vector with names: 'bias', 'lwr', 'upr'
#'
#' @export
add_BA_stats_geom_manual <- function(
  bias,
  lwr,
  upr,
  exponentiated = FALSE,
  name_ref = "ref",
  name_alt = "alt",
  line_labels = c(bias = "Bias", lwr = "95% LoA", upr = "95% LoA")
) {
  BA_stats <- as.data.frame(rbind(
    bias = bias[1:3],
    lwr = lwr[1:3],
    upr = upr[1:3]
  ))
  names(BA_stats) <- c("est", "ci.lwr", "ci.upr")

  if (!all(c("bias", "lwr", "upr") %in% names(line_labels))) {
    cli::cli_abort(
      "{.arg line_labels} must be a named vector with names: 'bias', 'lwr', 'upr'"
    )
  }
  BA_stats$label <- line_labels[rownames(BA_stats)]

  add_BA_stats_geom(
    BA_stats,
    exponentiated = exponentiated,
    name_alt = name_alt,
    name_ref = name_ref
  )
}

#' Plot BA estimates from log transformed data on raw data.
#'
#' @inheritParams plot_BA
#'
#' @return Bland-Altman style plot with relative differences plotted on absolute differences.
#'
#' @examples
#' BA_CO <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)
#' plot_BA_normalized_log(BA_CO)
#'
#' @export
plot_BA_normalized_log <- function(
  ba_obj,
  show_subject_legend = FALSE,
  aspect_ratio = NULL
) {
  assert_BA_obj(ba_obj)
  ba_obj_name <- deparse(substitute(ba_obj))
  check_CI(ba_obj, ba_obj_name)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (!data_is_log_transformed) {
    cli::cli_abort(
      "Plotting normalized log LoA is only valid for comparisons on the log-scale"
    )
  }

  # Generate data frame with BA statistics
  BA_stats <- gen_ba_stats_df(ba_obj)
  BA_stats <- BA_stats[BA_stats$stat %in% c("bias", "loa.lwr", "loa.upr"), ]

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

  BA_stats$slope <- log_estimate_to_avg_difference_slope(BA_stats$est)

  est_lines <- list(
    ggplot2::geom_abline(
      aes(slope = .data$slope, intercept = 0),
      linetype = 2,
      data = BA_stats
    ),

    ggplot2::geom_text(
      aes(
        x = set_limits(d$avg)[2],
        y = set_limits(d$avg)[2] * .data$slope,
        label = .data$label_w_val
      ),
      hjust = -0.05,
      data = BA_stats,
      inherit.aes = FALSE
    ),
    ggplot2::coord_cartesian(xlim = set_limits(d$avg), clip = "off")
  )

  # Add list of geoms for CIs
  if (is.null(ba_obj$BA_stats_ci)) {
    ci_shade <- NULL
  } else {
    # Create data from sloped ribbons
    ci_shade_df <- merge(
      BA_stats,
      data.frame(x = c(min(d$avg) * 0.95, max(d$avg) * 1.02))
    )

    ci_shade_df$ci.lwr <- ci_shade_df$x *
      log_estimate_to_avg_difference_slope(ci_shade_df$ci.lwr)
    ci_shade_df$ci.upr <- ci_shade_df$x *
      log_estimate_to_avg_difference_slope(ci_shade_df$ci.upr)

    ci_shade <- ggplot2::geom_ribbon(
      aes(
        x = .data$x,
        ymin = .data$ci.lwr,
        ymax = .data$ci.upr,
        group = .data$stat
      ),
      alpha = 0.5,
      fill = "gray",
      data = ci_shade_df,
      inherit.aes = FALSE
    )
  }

  if (is.numeric(aspect_ratio)) {
    plot_coord <- ggplot2::coord_fixed(ratio = aspect_ratio, clip = "off")
  } else {
    plot_coord <- ggplot2::coord_cartesian(clip = "off")
  }

  ggplot2::ggplot(
    d,
    aes(.data$avg, .data$diff, color = .data[[ba_obj$.var_names$id_col]])
  ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray") +
    ci_shade +
    est_lines +
    ggplot2::geom_point(show.legend = show_subject_legend) +
    create_axis_labels(ba_obj, normalized_log = TRUE) +
    plot_coord +
    theme_ba() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(1, 8, 1, 1, unit = "lines")
    )
}


#' Plot within-subject variation (model residuals) in differences and averages.
#'
#' @inheritParams plot_BA
#' @param show_sd Mark 1.96*SD (within-subject) on the plot (analouge to 95% LoA).
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#'
#' @export
plot_BA_residuals <- function(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE,
  show_sd = TRUE
) {
  assert_BA_obj(ba_obj)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (keep_log_scale && !data_is_log_transformed) {
    cli::cli_abort(
      "Data was not log transformed by {.fn compare_methods()}."
    )
  }

  diff_residuals <- stats::residuals(ba_obj$diff_model)
  avg_residuals <- stats::residuals(ba_obj$distribution_model)

  # Exponentiate data if relevant
  if (data_is_log_transformed && !keep_log_scale) {
    exponentiated <- TRUE
    diff_residuals <- exp(diff_residuals)
  } else {
    exponentiated <- FALSE
  }

  # Prepare plot settings
  plot_setup <- BA_plot_setup(
    exponentiated = exponentiated,
    data_is_log_transformed = data_is_log_transformed,
    aspect_ratio = aspect_ratio
  )

  BA_stats_geom <- NULL
  if (show_sd) {
    sd.within <- c(ba_obj$BA_stats$sd.within, ba_obj$BA_stats_ci$sd.within)

    upr <- 1.96 * sd.within
    lwr <- -1.96 * sd.within
    if (exponentiated) {
      upr <- exp(upr)
      lwr <- exp(lwr)
    }

    BA_stats_geom <- add_BA_stats_geom_manual(
      bias = NULL,
      lwr = lwr,
      upr = upr,
      line_labels = c(bias = "", lwr = "-1.96SD", upr = "+1.96SD")
    )
  }

  d <- ba_obj$data
  d$avg_residuals <- avg_residuals
  d$diff_residuals <- diff_residuals

  residual_plot <- ggplot2::ggplot(
    d,
    aes(
      avg_residuals,
      diff_residuals,
      color = .data[[ba_obj$.var_names$id_col]]
    )
  ) +
    plot_setup +
    BA_stats_geom[c("rect", "hline")] +
    ggplot2::geom_point(show.legend = show_subject_legend) +
    BA_stats_geom["label"] +
    create_axis_labels(ba_obj = ba_obj, exponentiated = exponentiated)

  # Update x-axis label to "Residual variation"
  current_label_x <- residual_plot$labels$x
  residual_plot$labels$x <- sub(
    "^Average",
    "Average (residual)",
    current_label_x
  )

  # Update y-axis label to "Residual difference"
  current_label_y <- residual_plot$labels$y
  residual_plot$labels$y <- sub(
    "^(Difference|Ratio)",
    "\\1 (residual)",
    current_label_y
  )

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
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE
) {
  assert_BA_obj(ba_obj)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (keep_log_scale && !data_is_log_transformed) {
    cli::cli_abort(
      "Data was not log transformed by {.fn compare_methods()}."
    )
  }

  var_names <- ba_obj$.var_names
  var_names_raw <- ba_obj$.var_names_raw

  if (keep_log_scale) {
    d <- ba_obj$data
    label_names <- var_names
  } else {
    d <- ba_obj$.non_log_data
    label_names <- var_names_raw
  }

  # Add units to label names
  measure_unit <- attr(ba_obj, "unit")
  if (!is.null(measure_unit)) {
    label_names$ref_col <- glue::glue("{label_names$ref_col} [{measure_unit}]")
    label_names$alt_col <- glue::glue("{label_names$alt_col} [{measure_unit}]")
  }

  plot_coord <- NULL

  if (is.numeric(aspect_ratio)) {
    plot_coord <- ggplot2::coord_fixed(ratio = aspect_ratio)
  }

  ggplot2::ggplot(
    d,
    aes(
      .data[[var_names_raw$ref_col]],
      .data[[var_names_raw$alt_col]],
      color = .data[[var_names$id_col]]
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray") +
    ggplot2::geom_point(show.legend = show_subject_legend) +
    ggplot2::labs(x = label_names$ref_col, y = label_names$alt_col) +
    plot_coord +
    theme_ba()
}

#' Plot all plots in extended Bland-Altman analysis.
#'
#' Creates a scatter plot, a standard Bland-Altman plot and a residuals plot for assessing trending ability.
#'
#' @inheritParams plot_BA
#' @param equal_scales Plot the residuals on a plane with the scale of the original data.
#' @param return_as_list Return the three plots in a list. If `FALSE`, the plots are combined using \{patchwork\}.
#' @param use_titles Show default titles on plots.
#'
#' @export
plot_BA_combine <- function(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  use_titles = TRUE,
  equal_scales = TRUE,
  keep_log_scale = FALSE,
  return_as_list = FALSE
) {
  assert_BA_obj(ba_obj)

  # Create scatter plot
  scatter_plot <- plot_BA_scatter(
    ba_obj = ba_obj,
    aspect_ratio = aspect_ratio,
    show_subject_legend = show_subject_legend,
    keep_log_scale = keep_log_scale
  )

  # Create Bland-Altman plot
  BA_plot <- plot_BA(
    ba_obj = ba_obj,
    show_subject_legend = show_subject_legend,
    aspect_ratio = aspect_ratio,
    keep_log_scale = keep_log_scale
  )

  # Create residuals plot
  residuals_plot <- suppressMessages(plot_BA_residuals(
    ba_obj = ba_obj,
    aspect_ratio = aspect_ratio,
    show_subject_legend = show_subject_legend,
    keep_log_scale = keep_log_scale
  ))

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

    if (ratio_scale) {
      y_scale <- ggplot2::scale_y_continuous(
        limits = exp(y_panel_limits),
        expand = c(0, 0),
        labels = scales::label_number(accuracy = 0.01),
        breaks = breaks_from_vec
      )
    } else {
      y_scale <- ggplot2::scale_y_continuous(
        limits = y_panel_limits,
        expand = c(0, 0)
      )
    }

    x_scale <- ggplot2::scale_x_continuous(
      limits = x_panel_limits - mean(x_panel_limits, expand = c(0, 0))
    )

    # Set scales for residual plot
    suppressMessages(
      # Avoid message about new plots
      residuals_plot <- residuals_plot +
        x_scale +
        y_scale
    )
  }

  # Default titles
  if (use_titles) {
    BA_plot <- BA_plot +
      ggplot2::labs(title = "Bland-Altman analysis")

    residuals_plot <- residuals_plot +
      ggplot2::labs(title = "Within-subject variation")
  }

  # Combine plots
  if (return_as_list) {
    list(
      scatter_plot = scatter_plot,
      BA_plot = BA_plot,
      residuals_plot = residuals_plot
    )
  } else {
    patchwork::wrap_plots(
      scatter_plot,
      BA_plot,
      residuals_plot,
      guides = "collect"
    )
  }
}

# Helper functions for plots
breaks_from_vec <- function(
  x,
  vec = c(0.33, 0.5, 0.66, 1, 1.5, 2, 3),
  fallback_fun = scales::breaks_log(base = 1.25, n = 5)
) {
  # Selects breaks from a predefined vector of desired breaks. If less than
  # 3 breaks are in range, use fallback function
  res <- vec[vec < max(x) & vec > min(x)]
  if (length(res) < 3) {
    fallback_fun(x)
  } else {
    res
  }
}

log_estimate_to_avg_difference_slope <- function(est) {
  # formula from https://doi.org/10.1016/j.jclinepi.2007.11.003
  2 * (exp(est) - 1) / (exp(est) + 1)
}

set_limits <- function(vec, rel_exp = 0.05, abs_exp = 0) {
  range_vec <- range(vec)
  expansion <- diff(range_vec) * rel_exp + abs_exp
  c(
    range_vec[1] - expansion,
    range_vec[2] + expansion
  )
}

create_axis_labels <- function(
  ba_obj,
  exponentiated = FALSE,
  normalized_log = FALSE
) {
  name_var_x <- ba_obj$.var_names_raw

  if (exponentiated || normalized_log) {
    name_var_y <- ba_obj$.var_names_raw
  } else {
    name_var_y <- ba_obj$.var_names
  }

  measure_unit <- attr(ba_obj, "unit")
  unit_str <- ""
  if (!is.null(measure_unit)) {
    unit_str <- glue::glue(" [{measure_unit}]")
  }

  # Create axis names
  x_name <- glue::glue(
    "Average{unit_str}
                   ({name_var_x$ref_col} + {name_var_x$alt_col}) / 2"
  )

  y_name <- if (exponentiated) {
    glue::glue(
      "Ratio
                        {name_var_y$alt_col} / {name_var_y$ref_col}"
    )
  } else {
    glue::glue(
      "Difference{unit_str}
                        {name_var_y$alt_col} - {name_var_y$ref_col}"
    )
  }

  ggplot2::labs(x = x_name, y = y_name)
}

check_CI <- function(ba_obj, ba_obj_name = "ba_obj") {
  # Check for CI
  if (is.null(ba_obj$BA_stats_ci)) {
    cli::cli_inform(
      c(
        i = "The Bland-Altman analysis object has no confidence intervals.",
        "To add confidence intervals run {.run {ba_obj_name} <- add_confint({ba_obj_name})}. (see {.fun BAtrending::add_confint} for help)"
      )
    )
  }
}
