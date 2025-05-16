test_that("format_single_est_ci works", {
  expect_equal(format_single_est_ci(est = 1, lwr = 0, upr = 2, decimals = 3, fmt_pct = FALSE), "1.000 [0.000; 2.000]")
  expect_equal(format_single_est_ci(est = 1, decimals = 1, fmt_pct = TRUE), "100.0 %")
})

testthat("format_est_ci works", {
  expect_equal(format_est_ci(1), "1.00")
  expect_equal(format_est_ci(1:2, 1:2, 1:2, fmt_pct = c(TRUE, FALSE)), c("100.0 [100.0; 100.0] %", "2.00 [2.00; 2.00]"))
  expect_error(format_est_ci(1, 1))
})