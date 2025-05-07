#' Compare methods
#'
#' @param df Data frame.
#' @param ref_col name of column containing reference measurements.
#' @param alt_col name of column containing alternative measurements.
#' @param id_col name of column containing unique subject id's.
#' @param REML Use restricted maximum likelihood optimization in `lme4::lmer()`.
#' @param logtrans Log-transform measurements before fitting the difference model.
#' @param logtrans_mean Log-transform measurements before fitting the mean model.
#'
#' @returns 
#' Bland Altman analysis object (of class ba_analysis)
#'
#' @export
#'
#' @examples
#' compare_methods(CO, ref_col = "rv", alt_col = "ic", id_col = "sub")
#' 
compare_methods <- function(df, ref_col, alt_col, id_col, REML = TRUE, logtrans = FALSE, logtrans_mean = FALSE) {
  if (!is.data.frame(df)) stop("df must be of class data.frame")
  if (logtrans_mean && !logtrans) warning("It likely does not make sense to logtransform the mean model (logtrans_mean = TRUE) 
  without logtransforming the difference model (logtrans = FALSE)")

  calc_mean_diff <- function(x_df){
    x_df$diff <- x_df[[alt_col]] - x_df[[ref_col]]
    x_df$mean <- (x_df[[alt_col]] + x_df[[ref_col]]) / 2
    x_df
  }

  # Convert id to factor
  df[[id_col]] <- factor(df[[id_col]])

  non_log_df <- df
  non_log_df <- calc_mean_diff(non_log_df)

  log_df <- df
  log_df[[ref_col]] <- log(df[[ref_col]])
  log_df[[alt_col]] <- log(df[[alt_col]])
  log_df <- calc_mean_diff(log_df)

  df <- if (logtrans) log_df else non_log_df

  diff_model <- lme4::lmer(stats::formula(paste0("diff ~ 1 + (1 | ", id_col, ")")),
                           REML = REML, data = df)
  
  mean_model <- lme4::lmer(stats::formula(paste0("mean ~ 1 + (1 | ", id_col, ")")),
                           REML = REML, 
                           data = if(logtrans_mean) log_df else non_log_df
                          )

  # Extract variance components
  BA_stats <- calc_BA_stats_from_model(diff_model)
  mean_stats <- calc_BA_stats_from_model(mean_model, incl_loa = FALSE)

  derived_BA_stats <- calc_derived_stats(BA_stats,
                                         mean_val = mean(df$mean),
                                         log = logtrans)

  BA_stats <- c(BA_stats, derived_BA_stats)

  structure(
    list(
    data = df,
    diff_model = diff_model,
    mean_model = mean_model,
    BA_stats = as.list(BA_stats),
    mean_stats = as.list(mean_stats),
    .var_names = list(
      ref_col = ifelse(logtrans, glue::glue("log({ref_col})"), ref_col),
      alt_col = ifelse(logtrans, glue::glue("log({alt_col})"), alt_col),
      id_col = id_col
    ),
    .raw_var_names = list(
      ref_col = ref_col,
      alt_col = alt_col,
      id_col = id_col
    ),
    .non_log_data = non_log_df
  ),
    class = "ba_analysis",
    logtrans = logtrans
  )
}

#' Add confidence intervals to BA analysis object. The confidence intervals are calculated with a percentile bootstrap (see `?lme4::confint.merMod` for details).
#'
#' @param ba_obj BA analysis object
#' @param level Confidence level (default is 0.95)
#' @param nsim Number of bootstrap samples
#' @param .progress see `?lme4::bootMer()`
#'
#' @return
#' BA analysis object (`x`) with added confidence intervals
#'
#' @export
add_confint <- function(ba_obj, level = 0.95, nsim = 1999, .progress = "txt") {
  stopifnot("ba_analysis" %in% class(ba_obj))
  BA_stats_ci <- confint.ba_analysis(ba_obj, level = level, nsim = nsim,  .progress = .progress)

  derived_BA_stats_ci <- apply(BA_stats_ci, 2, calc_derived_stats,
                               mean_val = mean(ba_obj$data$mean),
                               log = attr(ba_obj, "logtrans"), simplify = FALSE)

  derived_BA_stats_ci_mat <- as.matrix(
    data.frame(derived_BA_stats_ci, check.names = FALSE)
    )

  BA_stats_ci <- rbind(BA_stats_ci, derived_BA_stats_ci_mat)

  # Set names of the CI matrix to the respective confidence levels
  # (dimnames(BA_stats_ci)[[2]])
  #names(BA_stats_ci) <- rep(dimnames(BA_stats_ci)[[2]], each = dim(BA_stats_ci)[1])

  BA_stats_ci_list <- split(BA_stats_ci, factor(rownames(BA_stats_ci), levels = rownames(BA_stats_ci)))

  ba_obj$BA_stats_ci <- structure(BA_stats_ci_list, level = level)
  ba_obj
}

#' Calculate confidence interval
#'
#' @inheritParams add_confint
#'
#' @return
#' Matrix of bootstrap confidence intervals for BA statistics.
#'
#' @export
confint.ba_analysis <- function(ba_obj, level = 0.95, nsim = 1999, .progress = "txt") {
  message(glue::glue("Creating {nsim} bootstrap samples"))

  lme4::confint.merMod(ba_obj$diff_model,
                       method="boot",
                       FUN = calc_BA_stats_from_model,
                       level = level,
                       nsim = nsim,
                       .progress = .progress,
                       )
}

calc_BA_stats_from_model <- function(model, incl_loa = TRUE) {

  bias <- lme4::fixef(model) # Get intercept from model
  stopifnot(length(bias) == 1) # Test
  bias <- bias[[1]]


  # Get variance components (SD)
  varCorr_df <- as.data.frame(lme4::VarCorr(model))
  sd_components <- varCorr_df[["sdcor"]]
  names(sd_components) <- varCorr_df[["grp"]]

  stopifnot(length(sd_components) == 2) # Test

  sd.between <- unname(sd_components[1])
  sd.within <- unname(sd_components[2])
  sd.total <- sqrt(sd.between^2 + sd.within^2)

  intraclass.correlation = sd.between^2 / (sd.between^2 + sd.within^2)

  if (incl_loa) {
    loa <- c(
      loa.lwr = bias - 2*sd.total,
      loa.upr = bias + 2*sd.total
    )
  } else {
    loa <- NULL
  }

  c(bias = bias,
    sd.between = sd.between,
    sd.within = sd.within,
    sd.total = sd.total,
    intraclass.correlation = intraclass.correlation,

    loa

  )
}

gen_ba_stats_df <- function(ba_obj) {
  wide_stat_df <- rbind(ba_obj$BA_stats,
                        as.data.frame(ba_obj$BA_stats_ci))[1:3,] # Ensure 3 rows event with missing ci

  row.names(wide_stat_df) <- c("est", "ci.lwr", "ci.upr")
  long_stat_df <- as.data.frame(t(wide_stat_df))

  # Row names to stat column
  long_stat_df$stat <- row.names(long_stat_df)
  row.names(long_stat_df) <- NULL

  ba_labels <- c(bias = "Bias",
                 loa.lwr = "95% LoA",
                 loa.upr = "95% LoA")

  long_stat_df$label <- ba_labels[long_stat_df$stat]

  long_stat_df[,c(4,5,1:3)] # Reorder columns
}

# Calculate derived statistics, that are simply rescaled versions of the
# existing statistics. CI's can also simply be rescaled.
calc_derived_stats <- function(ba_stats, mean_val, log = FALSE) {

  trending.precision <- 2 * ba_stats["sd.within"]
  change.loa <- 2 * sqrt(2) * ba_stats["sd.within"]

  if (log) {
    # Percentage error is nonsensical for log transformed data
    percentage.error <- NULL
    percentage.trending.precision <- NULL
  } else {
    percentage.error <- 2*ba_stats["sd.total"] / mean_val
    percentage.trending.precision <- trending.precision / mean_val
  }


  c(
    change.loa = unname(change.loa),
    trending.precision = unname(trending.precision),
    percentage.error = unname(percentage.error),
    percentage.trending.precision = unname(percentage.trending.precision)

  )
}




