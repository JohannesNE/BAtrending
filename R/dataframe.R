#' Convert BA object to data.frame
#'
#' @param ba_obj
#'
#' @export
as.data.frame.ba_analysis <- function(ba_obj) {
    df <- gen_ba_stats_df(ba_obj)
    df$label <- NULL
    if (is.null(ba_obj$BA_stats_ci)) {
        df$ci.lwr <- NULL
        df$ci.upr <- NULL
    }
    df
}
