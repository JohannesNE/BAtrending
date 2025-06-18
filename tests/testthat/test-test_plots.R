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
      lwr = comp_co$BA_stats$loa.lwr,
      upr = comp_co$BA_stats$loa.upr
    )

  expect_doppelganger("Manual BA no CI", test_plot)
})

test_that("Add manual BA geom, Ratio w CI", {
  #Uses mean log(value) for x axis for simplicity (should optimally be .non-log-data$mean)
  test_plot <- ggplot2::ggplot(
    comp_co_log_w_ci$data,
    ggplot2::aes(mean, exp(diff))
  ) +
    ggplot2::geom_point() +
    add_BA_stats_geom_manual(
      bias = c(
        comp_co_log_w_ci$BA_stats$bias,
        comp_co_log_w_ci$BA_stats_ci$bias
      ) |>
        exp(),
      lwr = c(
        comp_co_log_w_ci$BA_stats$loa.lwr,
        comp_co_log_w_ci$BA_stats_ci$loa.lwr
      ) |>
        exp(),
      upr = c(
        comp_co_log_w_ci$BA_stats$loa.upr,
        comp_co_log_w_ci$BA_stats_ci$loa.upr
      ) |>
        exp(),
      exponentiated = TRUE,
      name_alt = "alternative"
    )

  expect_doppelganger("Manual BA, Ratio w CI", test_plot)
})

test_that("Standard residuals plot", {
  test_plot <- plot_BA_residuals(comp_co, show_sd = FALSE)
  expect_doppelganger("Standard residuals plot", test_plot)
})

test_that("Residuals plot on log data w ci", {
  test_plot <- plot_BA_residuals(comp_co_log_w_ci)
  expect_doppelganger("Residuals plot on log data", test_plot)
})

test_that("Combined plot with standard scale", {
  test_plot <- plot_BA_combine(comp_co)
  expect_doppelganger("Combined plot with standard scale", test_plot)
})

test_that("Combined plot with log scale", {
  test_plot <- plot_BA_combine(comp_co_log, keep_log_scale = TRUE)
  expect_doppelganger("Combined plot with log scale", test_plot)
})

test_that("Combined plot with ratio scale", {
  test_plot <- plot_BA_combine(comp_co_log)
  expect_doppelganger("Combined plot with ratio scale", test_plot)
})

test_that("Combined plot with legend", {
  test_plot <- plot_BA_combine(comp_co, show_subject_legend = TRUE)
  expect_doppelganger("Combined plot with legend", test_plot)
})

test_that("Combined plot with fixed aspect ratio", {
  test_plot <- plot_BA_combine(comp_co, aspect_ratio = 1)
  expect_doppelganger("Combined plot with fixed aspect ratio", test_plot)
})

test_that("Combined plot can return list", {
  test_plot_list <- plot_BA_combine(comp_co, return_as_list = TRUE)
  expect_type(test_plot_list, "list")
  expect_named(test_plot_list, c("scatter_plot", "BA_plot", "residuals_plot"))
})
