# Order stats and labels for table
ba_stat_labels <- c(
  bias = "Bias (alternative - reference)",
  sd.between = "Between subject variation (SD)",
  sd.within = "Within subject variation (SD)",
  sd.total = "Total variation (SD)",
  intraclass.correlation = "Intraclass correlation",
  loa.upr  = "\U2003 Upper limit",
  loa.lwr  = "\U2003 Lower limit",
  percentage.error = "Percentage error",
  trending.precision = "Trending precision (95%)",
  percentage.trending.precision = "Percentage trending precision (95%)",
  change.loa = "Change limits of agreement (95%)"
  
)


#' Create a table with the results of the Bland-Altman analysis
#'
#' @description
#' Uses {tinytable}.
#'
#' @param ba_obj An object created with `BAtrending::compare_methods()`. 
#' @param decimals A single numeric value specifying the number of decimal places for estimates and confidence intervals.
#' @param decimals_pct A single numeric value specifying the number of decimal places for percentage statistics.
#' @param keep_log_scale Show log transformed differences. If `FALSE` (default), values and parameters are exponentiated.
#'
#' @returns
#' A {tinytable} table of the estimates from the Bland-Altman analysis.
#'
#' @export
BA_table <- function(ba_obj, decimals = 2, decimals_pct = 1, keep_log_scale = FALSE) {
  assert_BA_obj(ba_obj)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (keep_log_scale && !data_is_log_transformed) cli::cli_abort("Data was not log transformed by {.fn compare_methods()}.")
  exponentiate <- data_is_log_transformed && !keep_log_scale

  # Use ba_stat_labels to initiate dataframe and set order
  ba_stat_labels_df <- data.frame(stat = names(ba_stat_labels), label = ba_stat_labels)
  ba_est <- as.data.frame(ba_obj)

  ba_est_full <- dplyr::left_join(ba_stat_labels_df, ba_est, by = "stat")

  # Format estimate and CI
  ba_est_full$est_ci <- format_est_ci(ba_est_full$est,
    lwr = ba_est_full$ci.lwr,
    upr = ba_est_full$ci.upr,
    fmt_pct = ba_est_full$stat %in% c("percentage.error", "percentage.trending.precision"),
    decimals = decimals, decimals_pct = decimals_pct,
    exponentiate = exponentiate
  )

  # Add ± to relevant stats
  range_symbol <- if (exponentiate) "⋇" else "±"
  ba_est_full$est_ci <- ifelse(ba_est_full$stat %in% c("change.loa", "trending.precision"),
    paste0(range_symbol, ba_est_full$est_ci),
    ba_est_full$est_ci
  )

  loa_group_label <- list("Limits of agreement (95%)" = which(ba_est_full$stat == "loa.upr"))
  
  tab_footnotes <- list(
    "a" = list(
      i = which(ba_est_full$stat == "trending.precision"),
      j = 1,
      text = "Trending precision (95%) = 2 * Within subject variation (SD)."
    ),
    "b" = list(
      i = which(ba_est_full$stat == "change.loa"),
      j = 1,
      text = "Change limits of agreement (95%) = √2 * Trending precision (95%)."
    )
  )
  
  est_label <- if(exponentiate) {
    "exp(logEstimate)"
  } else {
    "Estimate"
  }

  ci_label <- if(is.numeric(ba_est_full$ci.upr)) {
    " [95% CI]"
  } else {
    ""
  }

  est_ci_label <- paste0(est_label, ci_label)

  # Select relevant variables for table
  ba_table_df <- ba_est_full[, c("label", "est_ci")]
  ba_table_tt <- tinytable::tt(ba_table_df, notes = tab_footnotes)
  ba_table_tt <- tinytable::format_tt(ba_table_tt, 
    replace = "---")
  ba_table_tt <- tinytable::group_tt(ba_table_tt, i = loa_group_label, indent = 0)
  names(ba_table_tt) <- c("", est_ci_label)

  ba_table_tt
}

