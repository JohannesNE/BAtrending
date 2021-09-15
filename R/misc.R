assert_BA_obj <- function(obj) {
    if(!(inherits(obj, "ba_analysis"))) stop('`ba_obj` must be a "ba_analysis" object (from `compare_methods()`)')
}
