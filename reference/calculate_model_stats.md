# Calculate model statistics

Calculate model statistics

## Usage

``` r
calculate_model_stats(
  model_data,
  fit_coeffs_vec,
  glm_results = NULL,
  fit_algorithm = NULL,
  response = "yield",
  link = c("identity", "log"),
  type = c("theory", "raw"),
  Y = NULL,
  mu = NULL,
  n = NULL,
  npar = NULL,
  genome_factor = NULL,
  calc_type = c("fitting", "estimation"),
  model_family
)
```

## Arguments

- model_data:

  Data of the model.

- fit_coeffs_vec:

  Vector of fitting coefficients.

- glm_results:

  Results of glm.

- fit_algorithm:

  String of the algorithm used.

- response:

  Type of response.

- link:

  Fit link.

- type:

  Theoretical or raw glm model statistics.

- Y:

  Y response (required in constraint-maxlik-optimization).

- mu:

  mu response required in constraint-maxlik-optimization).

- n:

  number of parameters (required in constraint-maxlik-optimization).

- npar:

  number of parameters (required in constraint-maxlik-optimization).

- genome_factor:

  Genomic conversion factor used in translocations.

- calc_type:

  Calculation type, either "fitting" or "estimation".

- model_family:

  Model family.

## Value

Data frame of model statistics.
