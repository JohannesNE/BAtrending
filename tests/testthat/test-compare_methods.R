test_that("compare_methods works", {
    set.seed(1)
    comp_tmp <- compare_methods(CO, "ic", "rv", id_col = "sub")
    expect_equal(comp_tmp$BA_stats$bias, 0.704, tolerance=1e-3)
    expect_equal(comp_tmp$BA_stats$sd.total, 1.021, tolerance=1e-3)
})

test_that("compare_methods uses NSE", {
    set.seed(1)
    comp_tmp <- compare_methods(CO, ic, rv, id_col = sub)
    expect_equal(comp_tmp$BA_stats$bias, 0.704, tolerance=1e-3)
    expect_equal(comp_tmp$BA_stats$sd.total, 1.021, tolerance=1e-3)
})

test_that("compare_methods throws appropriate error", {
    expect_error(compare_methods(CO, CO, rv, id_col = sub), 'The column "CO" is missing from `df`')
})

test_that("confint works (not testing accuracy)", {
    set.seed(1)
    comp_co_w_ci_tmp <- suppressMessages(add_confint(comp_co, nsim = 100, .progress = "none"))


    expect_equal(unname(comp_co_w_ci_tmp$BA_stats_ci$loa.lwr), c(-2.20, -0.36), tolerance = 1)
})

test_that("gen_ba_stats_df works without ci", {
    stats_wo_ci <- gen_ba_stats_df(comp_co)

    expect_equal(stats_wo_ci$est[stats_wo_ci$stat == "bias"], 0.704521)
    expect_equal(stats_wo_ci$ci.upr[stats_wo_ci$stat == "bias"], NA_real_)
    expect_equal(nrow(stats_wo_ci), 9)

})

test_that("gen_ba_stats_df works with ci", {
    stats_w_ci <- gen_ba_stats_df(comp_co_w_ci)

    expect_equal(stats_w_ci$est[stats_w_ci$stat == "bias"], 0.704521)
    expect_equal(stats_w_ci$ci.upr[stats_w_ci$stat == "loa.upr"], 3.62749, tolerance = 1e-4)
})

