# Make scatter plot of paired measurements in analysis.

Make scatter plot of paired measurements in analysis.

## Usage

``` r
BA_plot_scatter(
  ba_obj,
  aspect_ratio = 1,
  square_plot = TRUE,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE
)
```

## Arguments

- ba_obj:

  Bland-Altman analysis object

- aspect_ratio:

  Set aspect ratio (x/y) between X and Y axis (sets
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)),
  Default (NULL) is automatic.

- square_plot:

  Use the same range for X and Y axes.

- show_subject_legend:

  Show legend for subjects

- keep_log_scale:

  Show log transformed differences. If `FALSE` (default), values and
  parameters are exponentiated before plotting
