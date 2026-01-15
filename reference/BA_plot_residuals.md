# Plot within-subject variation (model residuals) in differences and averages.

Plot within-subject variation (model residuals) in differences and
averages.

## Usage

``` r
BA_plot_residuals(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  keep_log_scale = FALSE,
  show_sd = TRUE
)
```

## Arguments

- ba_obj:

  Bland-Altman analysis object

- aspect_ratio:

  Set aspect ratio (x/y) between X and Y axis (sets
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)),
  Default (NULL) is automatic.

- show_subject_legend:

  Show legend for subjects

- keep_log_scale:

  Show log transformed differences. If `FALSE` (default), values and
  parameters are exponentiated before plotting

- show_sd:

  Mark 1.96\*SD (within-subject) on the plot (analouge to 95% LoA).
