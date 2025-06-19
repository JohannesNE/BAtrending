set.seed(1)

try(
  {
    comp_co <- compare_methods(CO, "ic", "rv", id_col = "sub")
  },
  silent = FALSE
)

try(
  {
    # Suppress messages and warnings as nsim = 10 will produce warnings due to too few samples.
    comp_co_w_ci <- suppressMessages(suppressWarnings(add_confint(
      comp_co,
      nsim = 10,
      .progress = "none"
    )))
  },
  silent = FALSE
)

try(
  {
    comp_co_log <- compare_methods(
      CO,
      "ic",
      "rv",
      id_col = "sub",
      logtrans = TRUE
    )
  },
  silent = FALSE
)

try(
  {
    comp_co_log_w_ci <- suppressMessages(suppressWarnings(add_confint(
      comp_co_log,
      nsim = 10,
      .progress = "none"
    )))
  },
  silent = FALSE
)
