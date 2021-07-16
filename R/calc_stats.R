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
compare_methods <- function(df, ref_col, alt_col, id_col) {
  if (!is.data.frame(df)) stop("df must be of class data.frame")

  df$diff <- df[[alt_col]] - df[[ref_col]]
  df$mean <- (df[[alt_col]] + df[[ref_col]]) / 2

  diff_model <- lme4::lmer(as.formula(paste0("diff ~ 1 + (1 | ", id_col, ")")), data = df)

  # Extract variance components
  varCorr_df <- as.data.frame(lme4::VarCorr(diff_model))
  sd_components <- varCorr_df[["sdcor"]]
  names(sd_components) <- varCorr_df[["grp"]]

  stopifnot(length(sd_components) == 2)

  structure(
    list(
    data = df,
    model = diff_model,
    var = list(inter_sd = sd_components[1],
              intra_sd = sd_components[2],
              total_sd = sqrt(sum(sd_components^2))
              )
  ),
    class = "ba_analysis"
  )
}

# Print method
#' @export
print.ba_analysis <- function(x) {
  ops <- options(digits = 3)
  on.exit(ops)

  n_obs <- x$model@devcomp$dims[["n"]]
  n_sub <- nlevels(x$model@flist[[1]])
  mean_bias <- lme4::fixef(x$model)

  cat(sprintf("%i paired measurements in %i subjects\n\n", n_obs, n_sub))



  cat("Variance components:\n")
  cat("Interindividual variance (SD):", x$var$inter_sd, "\n")
  cat("Intraindividual variance (SD):", x$var$intra_sd, "\n")
  cat("Total variance (SD)          :", x$var$total_sd, "\n\n")

  cat("Mean difference (alt - ref):", mean_bias, "\n")
  cat(sprintf("Limits of Agreement (95%%)  : [%.3f; %.3f]",
              mean_bias - 2 * x$var$total_sd,
              mean_bias + 2 * x$var$total_sd))

  invisible(x)
}

# Autoplot method

