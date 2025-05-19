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
#'
#' @returns
#' A {tinytable} table of the estimates from the Bland-Altman analysis.
#'
#' @export
BA_table <- function(ba_obj, decimals = 2, decimals_pct = 1) {
  assert_BA_obj(ba_obj)

  ba_est <- as.data.frame(ba_obj)
  ba_est$label <- ba_stat_labels[ba_est$stat]

  # Order rows after ba_stat_labels
  ba_est <- ba_est[match(names(ba_stat_labels), ba_est$stat), ]

  # Format estimate and CI
  ba_est$est_ci <- format_est_ci(ba_est$est,
    lwr = ba_est$ci.lwr,
    upr = ba_est$ci.upr,
    fmt_pct = ba_est$stat %in% c("percentage.error", "percentage.trending.precision"),
    decimals = decimals, decimals_pct = decimals_pct
  )

  # Add ± to relevant stats
  ba_est$est_ci <- ifelse(ba_est$stat %in% c("change.loa", "trending.precision"),
    paste0("±", ba_est$est_ci),
    ba_est$est_ci
  )

  loa_group_label <- list("Limits of agreement (95%)" = which(ba_est$stat == "loa.upr"))
  
  tab_footnotes <- list(
    "a" = list(
      i = which(ba_est$stat == "trending.precision"),
      j = 1,
      text = "Trending precision (95%) = 2 * Within subject variation (SD)."
    ),
    "b" = list(
      i = which(ba_est$stat == "change.loa"),
      j = 1,
      text = "Change limits of agreement (95%) = √2 * Trending precision (95%)."
    )
  )
  
  est_label <- if(is.numeric(ba_est$ci.upr)) {
    "Estimate [95% CI]"
  } else {
    "Estimate"
  }

  # Select relevant variables for table
  ba_table_df <- ba_est[, c("label", "est_ci")]
  ba_table_tt <- tinytable::tt(ba_table_df, notes = tab_footnotes)
  ba_table_tt <- tinytable::group_tt(ba_table_tt, i = loa_group_label, indent = 0)
  names(ba_table_tt) <- c("", est_label)

  ba_table_tt
}


format_est_ci <- function(est, lwr = NULL, upr = NULL, fmt_pct = FALSE, decimals = 2, decimals_pct = 1) {

  # Argument checks
  if (!is.numeric(est)) cli::cli_abort("{.var est} must be numeric.")
  if (length(est) == 0) cli::cli_abort("{.arg est} cannot be empty.")
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
 
  mapply(format_single_est_ci, est, lwr_arg, upr_arg, fmt_pct,
    MoreArgs = list(decimals = decimals, decimals_pct = decimals_pct), 
    USE.NAMES = FALSE
  )
}

# Function that formats a single set of est, lwr and upr. Later vectorized with mapply.
format_single_est_ci <- function(est, lwr = NULL, upr = NULL, fmt_pct = FALSE, decimals = 2, decimals_pct = 1) {

  has_full_ci <- !is.null(lwr) && !is.null(upr)

  if (has_full_ci) {
    # Format with Confidence Interval
    nums <- c(est, lwr, upr)
    
    if (fmt_pct) {
      nums_str <- format(round(100 * nums, decimals_pct), nsmall = decimals_pct)
      pattern <- "%s [%s; %s] %%" 
    } else {
      nums_str <- format(round(nums, decimals), nsmall = decimals)
      pattern <- "%s [%s; %s]" 
    }
  } else {
    # Format Estimate only
    if ( !(is.null(lwr) && is.null(upr)) ) stop("Only one CI limit was supplied.")

    nums <- c(est)
    
    if (fmt_pct) {
      nums_str <- format(round(100 * nums, decimals_pct), nsmall = decimals_pct)
      pattern <- "%s %%" 
    } else {
      nums_str <- format(round(nums, decimals), nsmall = decimals)
      pattern <- "%s" 
    }
  }

  # Call sprintf(pattern, nums_str) with potentially multiple numbers.
  return(do.call(sprintf, c(list(pattern), as.list(nums_str))))

}
