assert_BA_obj <- function(obj) {
  if (!inherits(obj, "ba_analysis")) {
    cli::cli_abort(
      "{.arg ba_obj} must be a {.cls ba_analysis} object (from {.fn compare_methods})."
    )
  }

  invisible(TRUE)
}

format_est_ci <- function(
  est,
  lwr = NULL,
  upr = NULL,
  fmt_pct = FALSE,
  decimals = 2,
  decimals_pct = 1,
  exponentiate = FALSE
) {
  # Argument checks
  if (!is.numeric(est)) {
    cli::cli_abort("{.var est} must be numeric.")
  }
  if (length(est) == 0) {
    cli::cli_abort("{.arg est} cannot be empty.")
  }
  if (xor(is.null(lwr), is.null(upr))) {
    cli::cli_abort(
      "If one of {.arg lwr} or {.arg upr} is provided, the other must also be.
     Or both can be NULL."
    )
  }

  n_est <- length(est)

  if (is.null(lwr) && is.null(upr)) {
    lwr_arg <- rep(list(NULL), n_est) # Create a list of NULLs
    upr_arg <- rep(list(NULL), n_est)
  } else {
    lwr_arg <- lwr
    upr_arg <- upr
  }

  mapply(
    format_single_est_ci,
    est = est,
    lwr = lwr_arg,
    upr = upr_arg,
    fmt_pct = fmt_pct,
    exponentiate = exponentiate,
    MoreArgs = list(
      decimals = decimals,
      decimals_pct = decimals_pct
    ),
    USE.NAMES = FALSE
  )
}

# Function that formats a single set of est, lwr and upr. Later vectorized with mapply.
format_single_est_ci <- function(
  est,
  lwr = NULL,
  upr = NULL,
  fmt_pct = FALSE,
  exponentiate = FALSE,
  decimals = 2,
  decimals_pct = 1
) {
  has_full_ci <- !is.null(lwr) && !is.null(upr)

  if (is.na(est)) {
    return(NA)
  }

  if (has_full_ci) {
    # Format with Confidence Interval
    nums <- c(est, lwr, upr)
    pattern <- "%s [%s; %s]"
  } else {
    # Format Estimate only
    if (!(is.null(lwr) && is.null(upr))) {
      cli::cli_abort("Only one CI limit was supplied.")
    }

    nums <- c(est)
    pattern <- "%s"
  }

  if (exponentiate) {
    nums <- exp(nums)
  }

  if (fmt_pct) {
    nums_str <- format(round(100 * nums, decimals_pct), nsmall = decimals_pct)
    pattern <- paste(pattern, "%%")
  } else {
    nums_str <- format(round(nums, decimals), nsmall = decimals)
  }

  # Call sprintf(pattern, nums_str) with potentially multiple numbers.
  return(do.call(sprintf, c(list(pattern), as.list(nums_str))))
}
