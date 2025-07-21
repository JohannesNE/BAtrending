#' Convert Bland-Altman analysis object to data.frame
#'
#' @param x Bland-Altman analysis object
#' @param ... not used
#'
#' @export
as.data.frame.ba_analysis <- function(x, ...) {
  df <- gen_ba_stats_df(x)
  df$label <- NULL
  if (is.null(x$BA_stats_ci)) {
    df$ci.lwr <- NULL
    df$ci.upr <- NULL
  }
  df
}
