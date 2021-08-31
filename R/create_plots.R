#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggplot2 aes
plot_BA <- function(x) {

    # Add list of geoms for CIs

    # Add list of geoms for est

    BA_plot <- ggplot2::ggplot(x$data, aes(mean, diff, color = .data[[x$.var_names$id_col]])) +
        # ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = bias_ci.lwr, ymax = bias_ci.upr), alpha = 0.2, fill = "blue",
        #           data = df_BA_stats, inherit.aes = FALSE) +
        # ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = loa.lwr_ci.lwr, ymax = loa.lwr_ci.upr), alpha = 0.2, fill = "blue",
        #           data = df_BA_stats, inherit.aes = FALSE) +
        # ggplot2::geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = loa.upr_ci.lwr, ymax = loa.upr_ci.upr), alpha = 0.2, fill = "blue",
        #           data = df_BA_stats, inherit.aes = FALSE) +
        # ggplot2::geom_hline(aes(yintercept = bias_est, linetype = 'Bias'), data = df_BA_stats) +
        # ggplot2::geom_hline(aes(yintercept = loa.lwr_est, linetype = '95% LOA'), data = df_BA_stats) +
        # ggplot2::geom_hline(aes(yintercept = loa.upr_est, linetype = '95% LOA'), data = df_BA_stats) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(0,1))) +
        labs(title = "Bland Altman plot",
             subtitle = "Absolute difference between methods",
             x = "mean(reference, experimental)",
             y = "difference\n (experimental - reference)")

    BA_plot
}
