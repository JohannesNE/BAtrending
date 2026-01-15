# Simulate measurement

Simulate measurement of a variable by adding random error. The function
allows adding a subject specific error (`sub_bias_sd`), that varies
between subjects.

## Usage

``` r
simulate_measurement(
  true_val,
  sub_id,
  residual_error_sd,
  mean_bias = 0,
  sub_bias_sd = 0,
  proportional_errors = FALSE
)
```

## Arguments

- true_val:

  A numeric vector of true values.

- sub_id:

  A factor or vector representing subject IDs. Must be the same length
  as var_true.

- residual_error_sd:

  Standard deviation of residual errors.

- mean_bias:

  Mean bias.

- sub_bias_sd:

  Standard deviation of subject-specific biases.

- proportional_errors:

  Should errors be proportional to true value. If TRUE, errors are added
  on a log scale, and the result is exponentiated. To add a bias of 10%
  (true_value \* 1.1), the mean_bias should be set to log(1.1). A SD of
  log(1.2) corresponds to ±20%.

## Value

A numeric vector representing simulated measurements.

## Examples

``` r
df <- simulate_repeated_data(n_sub = 50, n_rep = 5, var_name = "true_co")
df$ref <- simulate_measurement(
  df$true_co,
  df$id,
  mean_bias = log(1.1),
  sub_bias_sd = log(1.2),
  residual_error_sd = log(1.1),
  proportional_errors = TRUE
)
plot(df$true_co, df$ref, col=df$id)
abline(a=0, b=1)

```
