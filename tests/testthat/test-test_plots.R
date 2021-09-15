library(vdiffr)

test_that("Standard BA plot", {
    expect_message(
        test_plot <- plot_BA(comp_co),
        "has no confidence intervals"
    )
    expect_doppelganger("Standard BA no CI", test_plot)
})

test_that("Standard BA plot w CI", {
    test_plot <- plot_BA(comp_co_log_w_ci)
    expect_doppelganger("Standard BA w CI", test_plot)
})

test_that("Standard BA plot w CI", {
    test_plot <- plot_BA(comp_co_log_w_ci)
    expect_doppelganger("Standard BA w CI", test_plot)
})
