test_that("compare_methods works", {
    set.seed(1)
    comp_tmp <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)
    expect_equal(comp_tmp$BA_stats$bias, 0.145, tolerance=1e-2)
    expect_equal(comp_tmp$BA_stats$sd.combined, 0.202, tolerance=1e-2)
    expect_true(attr(comp_tmp, "logtrans"))
})

set.seed(1)
comp_co <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)

test_that("confint works (not testing accuracy)", {
    set.seed(1)
    comp_co_w_ci_tmp <- suppressMessages(add_confint(comp_co, nsim = 100))


    expect_equal(unname(comp_co_w_ci_tmp$BA_stats_ci$loa.lwr), c(-0.460, -0.095), tolerance = 1e-3)
})

set.seed(1)
comp_co_w_ci <- suppressMessages(add_confint(comp_co, nsim = 100))

test_that("gen_ba_stats_df works without ci", {
    stats_wo_ci <- gen_ba_stats_df(comp_co)

    expect_equal(stats_wo_ci$est[stats_wo_ci$stat == "bias"], 0.1454, tolerance = 1e-3)
    expect_equal(stats_wo_ci$ci.upr[stats_wo_ci$stat == "bias"], NA_real_)
    expect_equal(nrow(stats_wo_ci), 6)

})

test_that("gen_ba_stats_df works with ci", {
    stats_w_ci <- gen_ba_stats_df(comp_co_w_ci)

    expect_equal(stats_w_ci$est[stats_w_ci$stat == "bias"], 0.145, tolerance = 1e-2)
    expect_equal(stats_w_ci$ci.upr[stats_w_ci$stat == "loa.upr"], 0.734, tolerance = 1e-2)
})
