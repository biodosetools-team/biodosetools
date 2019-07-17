##### Data format

The only required data are:
- The doses `D` in Gy.
- The cell distributions of dicentrics `CX`, where `X` can be `0, 1, 2, 3...`.

The rest of columns will be calculated automatically:

- `N` is the total number of cells.
- `X` is the number of aberrations.
- `DI` is the dispersion index $\sigma^{2}/y$.
- `u` is the $u$-value, which for a Poisson distribution should be unity.
