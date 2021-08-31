test_that("compare methods works", {
    comp_co <- compare_methods(CO, "ic", "rv", id_col = "sub")
    expect_equal(comp_co$BA_stats$bias, 0.704, tolerance=1e-3)
    expect_equal(comp_co$BA_stats$sd.combined, 1.021, tolerance=1e-3)
})

