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



  cat("Variance components:\n")
  cat("Interindividual variance (SD):", x$BA_stats$sd.id, "\n")
  cat("Intraindividual variance (SD):", x$BA_stats$sd.residual, "\n")
  cat("Total variance (SD)          :", x$BA_stats$sd.combined, "\n\n")

  cat("Mean difference (alt - ref):", x$BA_stats$bias, "\n")
  cat(sprintf("Limits of Agreement (95%%)  : [%.3f; %.3f]",
              x$BA_stats$bias - 2 * x$BA_stats$sd.combined,
              x$BA_stats$bias + 2 * x$BA_stats$sd.combined))

  invisible(x)
}

# Autoplot method

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
