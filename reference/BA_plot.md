# Create Bland-Altman plot

Creates a standard Bland-Altman plot from a Bland-Altman analysis object
made with
[`compare_methods()`](https://johannesne.github.io/BAtrending/reference/compare_methods.md).

## Usage

``` r
BA_plot(
  ba_obj,
  aspect_ratio = NULL,
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

- show_subject_legend:

  Show legend for subjects

- keep_log_scale:

  Show log transformed differences. If `FALSE` (default), values and
  parameters are exponentiated before plotting

## Value

Bland-Altman plot (ggplot)

## See also

[`BA_plot_normalized_log()`](https://johannesne.github.io/BAtrending/reference/BA_plot_normalized_log.md)
which shows the results of a proportional Bland-Altman analysis (with
log-transformed measurements) on the non-transformed data.

## Examples

``` r
ba_obj <- compare_methods(CO, ic, rv, id_col = sub)
BA_plot(ba_obj)

```
