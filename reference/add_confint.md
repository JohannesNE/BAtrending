# Add confidence intervals to Bland-Altman analysis object.

The confidence intervals are calculated with a percentile parametric
bootstrap (see
[`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html)
for details).

## Usage

``` r
add_confint(
  ba_obj,
  level = 0.95,
  nsim = 1999,
  boot.type = c("perc", "basic", "norm"),
  .progress = "txt",
  PBargs = list(style = 3)
)
```

## Arguments

- ba_obj:

  Bland-Altman analysis object

- level:

  Confidence level (default is 0.95)

- nsim:

  Number of bootstrap samples

- boot.type:

  Type of interval (one of `"perc"` (default), `"basic"`, `"norm"`).
  Passed to
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html)

- .progress, PBargs:

  see [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html)

## Value

Bland-Altman analysis object (`x`) with added confidence intervals

## Details

Note that when calculating confidence intervals for percentage errors,
the mean value is treated as a known value rather than an estimate. Any
uncertainty in the estimate of the mean is not included in the
confidence intervals for percentage errors. This is a technical
limitation, since the bootstrap analysis is based on a single mixed
model (`diff ~ 1 + (1|id)`) which has no information about the mean
value.
