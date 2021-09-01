#' Compare methods
#'
#' @param df Data frame
#' @param ref_col name of column containing reference measurements
#' @param alt_col name of column containing alternative measurements
#' @param id_col name of column containing unique subject id's
#'
#' @return
#' Bland Altman analysis object (of class ba_analysis)
#'
#' @export
#'
#' @examples
#' compare_methods(CO, ref_col = "rv", alt_col = "ic", id_col = "sub")
compare_methods <- function(df, ref_col, alt_col, id_col, REML = TRUE) {
  if (!is.data.frame(df)) stop("df must be of class data.frame")

  df$diff <- df[[alt_col]] - df[[ref_col]]
  df$mean <- (df[[alt_col]] + df[[ref_col]]) / 2

  diff_model <- lme4::lmer(as.formula(paste0("diff ~ 1 + (1 | ", id_col, ")")),
                           REML = REML, data = df)

  # Extract variance components
  BA_stats <- calc_BA_stats_from_model(diff_model)

  structure(
    list(
    data = df,
    model = diff_model,
    BA_stats = as.list(BA_stats),
    .var_names = list(
      ref_col = ref_col,
      alt_col = alt_col,
      id_col = id_col
    )
  ),
    class = "ba_analysis"
  )
}

#' Add confidence intervals to BA analysis object
#'
#' @param ba_obj BA analysis object
#' @param level Confidence level (default is 0.95)
#' @param nsim Number of bootstrap samples
#'
#' @return BA analysis object (`x`) with added confidence intervals
#' @export
#'
#' @examples
add_confint <- function(ba_obj, level = 0.95, nsim = 2000) {
  stopifnot("ba_analysis" %in% class(ba_obj))
  BA_stats_ci <- confint.ba_analysis(ba_obj, level = level, nsim = nsim)

  # Set names of the CI matrix to the respective confidence levels
  # (dimnames(BA_stats_ci)[[2]])
  names(BA_stats_ci) <- rep(dimnames(BA_stats_ci)[[2]], each = dim(BA_stats_ci)[1])

  BA_stats_ci_list <- split(BA_stats_ci, dimnames(BA_stats_ci)[[1]])

  ba_obj$BA_stats_ci <- structure(BA_stats_ci_list, level = level)
  ba_obj
}

#' Calculate confidence interval
#'
#' @param ba_obj
#' @param level
#' @param nsim
#'
#' @return
#' Matrix of bootstrap confidence intervals for BA statistics.
#'
#' @export
confint.ba_analysis <- function(ba_obj, level = 0.95, nsim = 2000) {
  message(sprintf("Creating %i bootstrap samples", nsim))

  lme4::confint.merMod(ba_obj$model,
                       method="boot",
                       FUN = calc_BA_stats_from_model,
                       level = level,
                       nsim = nsim,
                       .progress = 'txt')
}

#' Print method
#' @export
print.ba_analysis <- function(ba_obj) {
  ops <- options(digits = 3)
  on.exit(options(ops))

  n_obs <- ba_obj$model@devcomp$dims[["n"]]
  n_sub <- nlevels(ba_obj$model@flist[[1]])

  cat(sprintf("%i paired measurements in %i subjects\n\n", n_obs, n_sub))

  # Create label for CI if CI exists
  if (is.null(ba_obj$BA_stats_ci)) {
    CI_label <- NULL
    }
  else {
    CI_label <- sprintf("     [%2g%% CI]", attr(ba_obj$BA_stats_ci, "level") * 100)
  }

  # Function that formats a single line of results
  format_line <- function(label, var) {
    cat(format(label, width = 30), ":",
        format(ba_obj$BA_stats[[var]], width = 6,
               nsmall = 3),
        sprintf("[% 2.3f; % 2.3f]", ba_obj$BA_stats_ci[[var]][1], ba_obj$BA_stats_ci[[var]][2]), "\n")
  }

  cat(format("", width = 30), "    est", CI_label, "\n")
  format_line("Bias (alt - ref)", "bias")
  format_line("Interindividual variance (SD)", "sd.id")
  format_line("Intraindividual variance (SD)", "sd.residual")
  format_line("Total variance (SD)", "sd.combined")
  cat("\n")
  cat("Limits of Agreement (95%)\n")
  format_line("├ Upper limit", "loa.upr")
  format_line("└ Lower limit", "loa.lwr")

  invisible(ba_obj)
}

calc_BA_stats_from_model <- function(model) {

  bias <- lme4::fixef(model) # Get intercept from model
  stopifnot(length(bias) == 1) # Test
  bias <- bias[[1]]


  # Get variance components (SD)
  varCorr_df <- as.data.frame(lme4::VarCorr(model))
  sd_components <- varCorr_df[["sdcor"]]
  names(sd_components) <- varCorr_df[["grp"]]

  stopifnot(length(sd_components) == 2) # Test

  sd_combined <- sqrt(sum(sd_components^2))

  c(bias = bias,
    sd.id = unname(sd_components[1]),
    sd.residual = unname(sd_components[2]),
    sd.combined = sd_combined,

    loa.lwr = bias - 2*sd_combined,
    loa.upr = bias + 2*sd_combined)
}
