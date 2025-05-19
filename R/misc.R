assert_BA_obj <- function(obj) {
    if(!inherits(obj, "ba_analysis")) {
        cli::cli_abort("{.arg ba_obj} must be a {.cls ba_analysis} object (from {.fn compare_methods}).")
    }

    invisible(TRUE)
}
