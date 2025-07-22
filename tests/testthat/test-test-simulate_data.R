test_that("simulation works", {
  set.seed(1)
  df <- simulate_repeated_data(n_sub = 50, n_rep = 5, var_name = "true_co")
  df$ref <- simulate_measurement(df$true_co, df$id, residual_error_sd = 1)
  expect_equal(sd(df$true_co - df$ref), 1, tolerance = 0.1)
})

test_that("log simulation works", {
  set.seed(1)
  df <- simulate_repeated_data(n_sub = 50, n_rep = 5, var_name = "true_co")
  df$ref <- simulate_measurement(
    df$true_co,
    df$id,
    mean_bias = log(1.1),
    sub_bias_sd = log(1.2),
    residual_error_sd = log(1.1),
    proportional_errors = TRUE
  )
  expect_equal(mean(df$ref / df$true), 1.1, tolerance = 0.1)
})
