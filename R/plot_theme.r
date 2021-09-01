theme_ba <- function(..., base_size = 12) {
    ggplot2::`%+replace%`(ggplot2::theme_minimal(base_size),
                          ggplot2::theme(panel.border = ggplot2::element_blank(),
                                         axis.line.x = ggplot2::element_line(size=.5, color = "black"),
                                         axis.line.y = ggplot2::element_line(size=.5, color = "black"),
                                         axis.text = ggplot2::element_text(size = rel(0.8)),
                                         panel.grid.minor = ggplot2::element_blank(),
                                         legend.key = ggplot2::element_blank(),
                                         plot.margin=ggplot2::unit(c(4,4,1,1),"mm"), ...))
}
