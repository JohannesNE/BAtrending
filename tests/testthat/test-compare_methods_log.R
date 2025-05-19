test_that("compare_methods works", {
    set.seed(1)
    comp_tmp <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)
    expect_equal(comp_tmp$BA_stats$bias, 0.145, tolerance=1e-2)
    expect_equal(comp_tmp$BA_stats$sd.total, 0.202, tolerance=1e-2)
    expect_true(attr(comp_tmp, "logtrans"))
})

test_that("gen_ba_stats_df works without ci", {
    stats_wo_ci <- BAtrending:::gen_ba_stats_df(comp_co_log)

    expect_equal(stats_wo_ci$est[stats_wo_ci$stat == "bias"], 0.1454, tolerance = 1e-3)
    expect_equal(stats_wo_ci$ci.upr[stats_wo_ci$stat == "bias"], NA_real_)
    expect_equal(nrow(stats_wo_ci), 10)

})

