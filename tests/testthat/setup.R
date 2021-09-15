set.seed(1)

try({
    comp_co <- compare_methods(CO, "ic", "rv", id_col = "sub")
    comp_co_w_ci <- suppressMessages(add_confint(comp_co, nsim = 100, .progress = "none"))

    comp_co_log <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)
    comp_co_log_w_ci <- suppressMessages(add_confint(comp_co_log, nsim = 100, .progress = "none"))

}, silent = TRUE)
