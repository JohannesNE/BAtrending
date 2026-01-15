# Plot all plots in extended Bland-Altman analysis.

Creates a scatter plot, a standard Bland-Altman plot and a residuals
plot for assessing trending ability.

## Usage

``` r
BA_plot_combine(
  ba_obj,
  aspect_ratio = NULL,
  show_subject_legend = FALSE,
  use_titles = TRUE,
  equal_scales = TRUE,
  keep_log_scale = FALSE,
  return_as_list = FALSE
)
```

## Arguments

- ba_obj:

  Bland-Altman analysis object

- aspect_ratio:

  Set aspect ratio (x/y) between X and Y axis (sets
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)),
  Default (NULL) is automatic. This only applies to the BA plots. The
  scatter plot always have an aspect ratio of 1.

- show_subject_legend:

  Show legend for subjects

- use_titles:

  Show default titles on plots.

- equal_scales:

  Plot the residuals on a plane with the scale of the original data.

- keep_log_scale:

  Show log transformed differences. If `FALSE` (default), values and
  parameters are exponentiated before plotting

- return_as_list:

  Return the three plots in a list. If `FALSE`, the plots are combined
  using {patchwork}.
