
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DVI app

The purpose of this app is to provide easy functionality for DVI …

## Built in data

The following data documented in dvir:

-   Tutorial example (example1 in dvir)
-   grave
-   planecrash

## File formats for input

-   RData, on format as for the built in data
-   Familias file (not likely to be fully implemented)

## Functionality

-   IBD estimates. Performed on PM data. LR gives test for estimated
    relationship as given by k-s to pedigree relationship (unrelated)
    given by kappa-s
-   Exclusion Counts number of exclusions for PM sample
-   Pairwise LR comparing pm sample in MP position to unrelated
-   Joint. Also called `global`
-   Posterior. For each pm sample with flat prior
