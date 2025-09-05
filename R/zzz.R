# Set tinytable option to render mathjax in html
.onLoad <- function(libname, pkgname) {
  options(tinytable_html_mathjax = TRUE)
  cli::cli_inform(list(i = "Setting `options(tinytable_html_mathjax = TRUE)`"))

  invisible()
}
