library(vdiffr)

test_that("Standard BA plot", {
    expect_message(
        test_plot <- plot_BA(comp_co),
        "has no confidence intervals"
    )
    expect_doppelganger("Standard BA no CI", test_plot)
})

test_that("Standard BA plot w CI", {
    test_plot <- plot_BA(comp_co_w_ci)
    expect_doppelganger("Standard BA plot w CI", test_plot)
})

test_that("Standard BA plot on log data w CI", {
    test_plot <- plot_BA(comp_co_log_w_ci, keep_log_scale = TRUE)
    expect_doppelganger("Standard BA plot on log data w CI", test_plot)
})

test_that("Ratio BA plot w CI", {
    test_plot <- plot_BA(comp_co_log_w_ci, keep_log_scale = FALSE)
    expect_doppelganger("Ratio BA w CI", test_plot)
})

test_that("normalized log plot w ci", {
        test_plot <- plot_BA_normalized_log(comp_co_log_w_ci)
    expect_doppelganger("normalized log plot w ci", test_plot)
})

test_that("Add manual BA geom, no CI", {
    test_plot <- ggplot2::ggplot(comp_co$data, ggplot2::aes(mean, diff)) +
        ggplot2::geom_point() +
        add_BA_stats_geom_manual(
            bias = comp_co$BA_stats$bias,
            loa.lwr = comp_co$BA_stats$loa.lwr,
            loa.upr = comp_co$BA_stats$loa.upr
        )

    expect_doppelganger("Manual BA no CI", test_plot)
})

test_that("Add manual BA geom, Ratio w CI", {
    #Uses mean log(value) for x axis for simplicity (should optimally be .non-log-data$mean)
    test_plot <- ggplot2::ggplot(comp_co_log_w_ci$data, ggplot2::aes(mean, exp(diff))) +
        ggplot2::geom_point() +
        add_BA_stats_geom_manual(
            bias = c(comp_co_log_w_ci$BA_stats$bias, comp_co_log_w_ci$BA_stats_ci$bias) |> exp(), 
            loa.lwr = c(comp_co_log_w_ci$BA_stats$loa.lwr, comp_co_log_w_ci$BA_stats_ci$loa.lwr) |> exp(), 
            loa.upr = c(comp_co_log_w_ci$BA_stats$loa.upr, comp_co_log_w_ci$BA_stats_ci$loa.upr) |> exp(),
            exponentiated = TRUE,
            name_alt = "alternative"
        )

    expect_doppelganger("Manual BA, Ratio w CI", test_plot)
})

test_that("Standard residuals plot", {
    test_plot <- plot_BA_residuals(comp_co)
    expect_doppelganger("Standard residuals plot", test_plot)
})

test_that("Residuals plot on log data", {
    test_plot <- plot_BA_residuals(comp_co_log)
    expect_doppelganger("Residuals plot on log data", test_plot)
})