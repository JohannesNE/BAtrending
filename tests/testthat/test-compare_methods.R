test_that("compare_methods works", {
    comp_tmp <- compare_methods(CO, "ic", "rv", id_col = "sub")
    expect_equal(comp_tmp$BA_stats$bias, 0.704, tolerance=1e-3)
    expect_equal(comp_tmp$BA_stats$sd.combined, 1.021, tolerance=1e-3)
})

comp_co <- compare_methods(CO, "ic", "rv", id_col = "sub")

test_that("confint works (not testing accuracy)", {
    comp_co_w_ci_tmp <- add_confint(comp_co, nsim = 100)

    expect_equal(unname(comp_co_w_ci_tmp$BA_stats_ci$loa.lwr), c(-2.20, -0.36), tolerance = 1)
})

comp_co_w_ci_tmp <- add_confint(comp_co, nsim = 100)
