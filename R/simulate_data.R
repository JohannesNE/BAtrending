#' Simulate repeated data
#'
#' Simulates `n_rep` repeated values in `n_sub` subjects.
#'
#' @param n_sub A single number specifying the number of subjects.
#' @param n_rep A single number specifying the number of repetitions per subject.
#' @param avg A single number specifying the average value across subjects.
#' @param between_sub_sd A single number specifying the standard deviation between subjects.
#' @param within_sub_rel_change_sd A single number controlling the within-subject relative change variability.
#' @param var_name A single string specifying the name of the simulated variable column.
#'
#' @return
#' A data frame containing simulated data with `id` and the simulated variable.
#'
#' @export
simulate_repeated_data <- function(
  n_sub = 50,
  n_rep = 5,
  avg = 5,
  between_sub_sd = 0.5,
  within_sub_rel_change_sd = 0.3,
  var_name = "co_true"
) {
  mean_val <- rep(stats::rnorm(n_sub, avg, between_sub_sd), each = n_rep)
  within_sub_rel_change <- stats::rlnorm(
    n_sub * n_rep,
    0,
    within_sub_rel_change_sd
  )

  res <- data.frame(
    id = as.factor(rep(1:n_sub, each = n_rep)),
    co_true = mean_val * within_sub_rel_change
  )

  names(res)[2] <- var_name

  res
}

#' Simulate measurement
#'
#' Simulate measurement of a variable by adding random error.
#' The function allows adding a subject specific error (`sub_bias_sd`), that varies between subjects.
#'
#' @param true_val A numeric vector of true values.
#' @param sub_id A factor or vector representing subject IDs. Must be the same length as var_true.
#' @param mean_bias Mean bias.
#' @param sub_bias_sd Standard deviation of subject-specific biases.
#' @param residual_error_sd Standard deviation of residual errors.
#' @param proportional_errors Should errors be proportional to true value.
#' If TRUE, errors are added on a log scale, and the result is exponentiated. To add a bias of 10% (true_value * 1.1),
#' the mean_bias should be set to log(1.1). A SD of log(1.2) corresponds to ±20%.
#'
#' @return A numeric vector representing simulated measurements.
#'
#' @examples
#' df <- simulate_repeated_data(n_sub = 50, n_rep = 5, var_name = "true_co")
#' df$ref <- simulate_measurement(
#'   df$true_co,
#'   df$id,
#'   mean_bias = log(1.1),
#'   sub_bias_sd = log(1.2),
#'   residual_error_sd = log(1.1),
#'   proportional_errors = TRUE
#' )
#' plot(df$true_co, df$ref, col=df$id)
#' abline(a=0, b=1)
#'
#' @export
simulate_measurement <- function(
  true_val,
  sub_id,
  residual_error_sd,
  mean_bias = 0,
  sub_bias_sd = 0,
  proportional_errors = FALSE
) {
  stopifnot(length(true_val) == length(sub_id))
  stopifnot(all(residual_error_sd >= 0, sub_bias_sd >= 0))

  n_obs <- length(true_val)
  unique_sub <- levels(sub_id)
  n_sub <- length(unique_sub)

  sub_bias <- stats::rnorm(n_sub, mean = 0, sd = sub_bias_sd)

  # repeats the bias vector to match the sub vector.
  sub_bias_vec <- sub_bias[as.integer(sub_id)]

  residual_error_vec <- stats::rnorm(n_obs, mean = 0, sd = residual_error_sd)

  if (proportional_errors) {
    res <- exp(log(true_val) + mean_bias + sub_bias_vec + residual_error_vec)
  } else {
    res <- true_val + mean_bias + sub_bias_vec + residual_error_vec
  }

  # Return
  res
}
