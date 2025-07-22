# Order stats and labels for table
ba_stat_labels <- c(
  distr.mean = "Mean",
  distr.sd.between = "Between-subject SD",
  distr.sd.within = "Within-subject SD",
  distr.sd.total = "Total SD",
  bias = "Bias",
  sd.between = "Between-subject SD",
  sd.within = "Within-subject SD",
  sd.total = "Total SD",
  # intraclass.correlation = "Intraclass correlation",
  loa.upr = "\U2003 Upper limit",
  loa.lwr = "\U2003 Lower limit",
  percentage.error = "Percentage error",
  percentage.error.within = "Within-subject percentage error",
  change.loa = "Change limits of agreement (95%)"
)


#' Create a table with the results of the Bland-Altman analysis
#'
#' Uses [tinytable:tt()] to create a publication-ready table of the results of the Bland-Altman analysis.
#' By default, results from a log-transformed analysis is exponentiated.
#'
#' @param ba_obj An object created with `BAtrending::compare_methods()`.
#' @param decimals A single numeric value specifying the number of decimal places for estimates and confidence intervals.
#' @param decimals_pct A single numeric value specifying the number of decimal places for percentage statistics.
#' @param keep_log_scale Show log transformed differences. If `FALSE` (default), values and parameters are exponentiated.
#'
#' @returns
#' A table of the estimates from the Bland-Altman analysis.
#'
#' @export
BA_table <- function(
  ba_obj,
  decimals = 2,
  decimals_pct = 1,
  keep_log_scale = FALSE
) {
  assert_BA_obj(ba_obj)

  data_is_log_transformed <- attr(ba_obj, "logtrans")
  if (keep_log_scale && !data_is_log_transformed) {
    cli::cli_abort("Data was not log transformed by {.fn compare_methods()}.")
  }
  exponentiate <- data_is_log_transformed && !keep_log_scale

  ba_df <- BA_table_df(
    ba_obj,
    decimals = decimals,
    decimals_pct = decimals_pct,
    exponentiate = exponentiate
  )

  BA_table_tt(
    ba_df,
    exponentiated = exponentiate,
    is_log_trans = data_is_log_transformed
  )
}

BA_table_df <- function(
  ba_obj,
  decimals = 2,
  decimals_pct = 1,
  exponentiate = FALSE
) {
  assert_BA_obj(ba_obj)

  # Use ba_stat_labels to initiate dataframe and set order
  ba_stat_labels_df <- data.frame(
    stat = names(ba_stat_labels),
    label = ba_stat_labels
  )
  ba_est <- as.data.frame(ba_obj)

  ba_est_full <- merge(
    ba_stat_labels_df,
    ba_est,
    by = "stat",
    all.x = TRUE,
    sort = FALSE
  )

  # Sort to match order of labels
  ba_est_full <- ba_est_full[match(ba_stat_labels_df$stat, ba_est_full$stat), ]

  # Format estimate and CI
  ba_est_full$est_ci <- format_est_ci(
    ba_est_full$est,
    lwr = ba_est_full$ci.lwr,
    upr = ba_est_full$ci.upr,
    fmt_pct = ba_est_full$stat %in%
      c("percentage.error", "percentage.error.within"),
    decimals = decimals,
    decimals_pct = decimals_pct,
    exponentiate = exponentiate & !grepl("^distr", ba_est_full$stat) # Do not exponentiate stats that start with "distr"
  )

  # Add ± to relevant stats
  range_symbol <- if (exponentiate) "⋇" else "±"
  ba_est_full$est_ci <- ifelse(
    ba_est_full$stat %in% c("change.loa"),
    paste0(range_symbol, ba_est_full$est_ci),
    ba_est_full$est_ci
  )

  # Add column names
  est_label <- "Estimate"

  ci_label <- if (is.numeric(ba_est_full$ci.upr)) {
    " [95% CI]"
  } else {
    ""
  }

  est_ci_label <- paste0(est_label, ci_label)

  # Select relevant variables for table
  ba_table_df <- ba_est_full[, c("stat", "label", "est_ci")]
  names(ba_table_df)[3] <- est_ci_label

  ba_table_df
}

BA_table_tt <- function(ba_df, exponentiated = FALSE, is_log_trans = FALSE) {
  loa_group_label <- list(
    "**Distribution**¹" = which(ba_df$stat == "distr.mean"),
    "**Method comparison (alt - ref)**" = which(
      ba_df$stat == "bias"
    ),
    "Limits of agreement (95%)" = which(ba_df$stat == "loa.upr")
  )

  footnote_abbreviations <- "SD: standard deviation; alt: alternative method; ref: reference method"

  if (is_log_trans) {
    if (exponentiated) {
      names(loa_group_label)[2] <-
        "**Method comparison, exp(log-analysis), (alt / ref)**"
      footnote_abbreviations <- paste0(
        footnote_abbreviations,
        "; ⋇: multiply or divide"
      )
    } else {
      names(loa_group_label)[2] <-
        "**Method comparison (log(alt) - log(ref))**"
    }
  }

  tab_footnotes <- list(
    # It is currently not possible to automatically set at footnote on a label.
    "1" = "Distribution of the averages of simultaneous measurements (alt + ref)/2.",
    "2" = list(
      i = which(
        ba_df$stat %in% c("percentage.error", "percentage.error.within")
      ),
      j = 1,
      text = "Percentage error = 1.96 · Total (or Within-subject) SD/mean"
    ),
    "3" = list(
      i = which(ba_df$stat == "change.loa"),
      j = 1,
      text = "Change limits of agreement (95%) = 1.96 · √2 · Within-subject SD."
    ),
    footnote_abbreviations
  )

  ba_df_clean <- subset(ba_df, select = -stat)
  names(ba_df_clean)[1] <- ""

  ba_table_tt <- tinytable::tt(ba_df_clean, notes = tab_footnotes)
  ba_table_tt <- tinytable::format_tt(ba_table_tt, replace = "--") # Replace NA with --
  ba_table_tt <- tinytable::group_tt(
    ba_table_tt,
    i = loa_group_label,
    indent = 0
  )
  ba_table_tt <- tinytable::format_tt(
    ba_table_tt,
    markdown = TRUE,
    escape = TRUE # Escape characters (importat for %)
  )

  ba_table_tt
}
