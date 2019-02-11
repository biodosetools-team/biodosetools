#### Fitting results data format

The `RDS` format creates a serialized version of any R object and then saves it with `gzip` compression. This ensures there is no loss of data in saving the object with fitting results, which contain:

- Original data set used for the calculation.
- Model used for the fitting.
- Calculated coefficients.
- Model-level statistics.
- Etc.

#### Report

The report contains the results of the curve fit and the summary statistics, and curve plot.

Additionally, it allows the user to download the `RDS` object with the fitting results.
