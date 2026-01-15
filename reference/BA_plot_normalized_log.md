# Plot relative Bland-Altman estimates on raw data.

Visualizes a Bland-Altman analysis with log-transformed data (showing
relative errors) on the non-transformed data.

## Usage

``` r
BA_plot_normalized_log(
  ba_obj,
  show_subject_legend = FALSE,
  aspect_ratio = NULL
)
```

## Arguments

- ba_obj:

  Bland-Altman analysis object

- show_subject_legend:

  Show legend for subjects

- aspect_ratio:

  Set aspect ratio (x/y) between X and Y axis (sets
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)),
  Default (NULL) is automatic.

## Value

Bland-Altman style plot with relative differences plotted on absolute
differences.

## Examples

``` r
BA_CO <- compare_methods(CO, "ic", "rv", id_col = "sub", logtrans = TRUE)
BA_plot_normalized_log(BA_CO)

```
