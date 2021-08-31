#' Compare methods
#'
#' @param df Data frame
#' @param ref_col name of column containing reference measurements
#' @param alt_col name of column containing alternative measurements
#' @param id_col name of column containing unique subject id's
#'
#' @return
#' @export
#'
#' @examples compare_methods(CO, ref_col = "rv", alt_col = "ic", id_col = "sub")
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
    BA_stats = as.list(BA_stats)
  ),
    class = "ba_analysis"
  )
}

#' Add confidence intervals to BA analysis object
#'
#' @param x BA analysis object
#' @param level Confidence level (default is 0.95)
#' @param nsim Number of bootstrap samples
#'
#' @return BA analysis object (`x`) with added confidence intervals
#' @export
#'
#' @examples
add_confint <- function(x, level = 0.95, nsim = 2000) {
  stopifnot("ba_analysis" %in% class(x))
  BA_stats_ci <- confint.ba_analysis(x, level = level, nsim = nsim)

  # Set names of the CI matrix to the respective confidence levels
  # (dimnames(BA_stats_ci)[[2]])
  names(BA_stats_ci) <- rep(dimnames(BA_stats_ci)[[2]], each = dim(BA_stats_ci)[1])

  BA_stats_ci_list <- split(BA_stats_ci, dimnames(BA_stats_ci)[[1]])

  x$BA_stats_ci <- structure(BA_stats_ci_list, level = level)
  x
}

#' Calculate confidence interval
#' @export
confint.ba_analysis <- function(x, level = 0.95, nsim = 2000) {
  message(sprintf("Creating %i bootstrap samples", nsim))

  lme4::confint.merMod(x$model,
                       method="boot",
                       FUN = calc_BA_stats_from_model,
                       level = level,
                       nsim = nsim,
                       .progress = 'txt')
}

#' Print method
#' @export
print.ba_analysis <- function(x) {
  ops <- options(digits = 3)
  on.exit(options(ops))

  n_obs <- x$model@devcomp$dims[["n"]]
  n_sub <- nlevels(x$model@flist[[1]])

  cat(sprintf("%i paired measurements in %i subjects\n\n", n_obs, n_sub))

  if (is.null(x$BA_stats_ci)) {
    CI_label <- NULL
    }
  else {
    CI_label <- sprintf("     [%2g%% CI]", attr(x$BA_stats_ci, "level") * 100)
  }

  format_interval <- function(vec) sprintf("[%.3f; %.3f]", vec[1], vec[2])

  cat("Variance components:           est", CI_label, "\n")
  cat("Interindividual variance (SD):", x$BA_stats$sd.id,
      format_interval(x$BA_stats_ci$sd.id), "\n")
  cat("Intraindividual variance (SD):", x$BA_stats$sd.residual, "\n")
  cat("Total variance (SD)          :", x$BA_stats$sd.combined, "\n\n")

  cat("Mean difference (alt - ref):", x$BA_stats$bias, "\n")
  cat(sprintf("Limits of Agreement (95%%)  : [%.3f; %.3f]",
              x$BA_stats$bias - 2 * x$BA_stats$sd.combined,
              x$BA_stats$bias + 2 * x$BA_stats$sd.combined))

  invisible(x)
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
