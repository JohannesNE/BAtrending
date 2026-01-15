# Manually add Bland-Altman geometry to plot

Manually add Bland-Altman geometry to plot

## Usage

``` r
add_BA_stats_geom_manual(
  bias,
  lwr,
  upr,
  exponentiated = FALSE,
  name_ref = "ref",
  name_alt = "alt",
  line_labels = c(bias = "Bias", lwr = "95% LoA", upr = "95% LoA")
)
```

## Arguments

- bias, lwr, upr:

  estimates to be plotted. Optionally including confidence intervals as
  `c(est, ci.lwr, ci.upr)`.

- exponentiated:

  Set true if estimates are exponentiated estimates from a model on
  log-transformed data. Treats estimates as ratios.

- name_ref, name_alt:

  Name of reference and alternative method. Only used if
  `exponentiated = TRUE`

- line_labels:

  Labels for Bias and LoA lines. Must be a named vector with names:
  'bias', 'lwr', 'upr'
