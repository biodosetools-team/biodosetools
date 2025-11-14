# Plot triage

Plot triage

## Usage

``` r
plot_triage(num_cases, est_doses_whole, est_doses_partial, assessment, place)
```

## Arguments

- num_cases:

  number of cases.

- est_doses_whole:

  dose estimation list of cases.

- est_doses_partial:

  dose estimation list of cases.

- assessment:

  partial or whole.

- place:

  UI or save.

## Value

`ggplot2` object.

## Examples

``` r
est_doses_whole <- data.frame(
    ID = c("example1", "example2"),
    lower = c(1.407921, 2.604542),
    estimate = c(1.931245, 3.301014),
    upper = c(2.632199, 4.221749)
  )


plot_triage(
  num_cases = 2,
  est_doses_whole ,
  est_doses_partial = NULL,
  assessment = "whole",
  place = "UI"
)

```
