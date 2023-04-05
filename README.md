# inequality_in_shock_spreading

Minimal code for arXiv:2112.00415 in R and C++. Used for running the DebtRank style cascade and aggregating it to the average exposure matrix Ecd. Provided with the toy example from Fig. 1 as minimal code.

## Libraries:
- RcppArmadillo
- Rcpp
- Matrix
- data.table


## How to use:

Terminal:
```
Rscript 01-calculate_Dij.R
Rscript 02-aggregate_Dij.R
```
Or execute the scripts in Rstudio.
