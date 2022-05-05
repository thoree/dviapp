
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DVI app

The purpose of the app is to provide easy functionality for power
calculations, selecting optimal individuals to genotype and DVI
functionality.

The options of the app are described next.

## Built in pedigree for power simulation

Some pedigrees available. There are 35 markers from the
\`NorwegianFrequency’ (availiable in R library forrel) database, none
are typed.

## NorwegianFrequency markers

The markers 1,…, n, where n=35 or smaller, can be chosen.

## Familias file for simulation

The file is created on beforehand in the main module of Familias, not
the DVI module. There are no restrictions on the database of allele
frequencies. There should be only one pedigree. The missing person
should be called `Missing`, the reference `REF`, and the candidates for
typing `E1`and `E2`. Mutation and pother artefact (theta correction,
silent allele, drop-out, is ignored in the simulation part.)

## Built in data

The following data documented in dvir is provided:

-   Tutorial example (example1 in dvir)
-   grave
-   planecrash

## User data for DVI

-   RData, on format as for the built in data
-   Familias file

## Choose DVI analysis

Summary of data and plot of reference family (if there are several, the
one to plot is chosen in `Reference family to plot`) is done and
displayed as data is loaded.

-   IBD estimates. Performed on PM data. LR gives test for estimated
    relationship as given by k-s to pedigree relationship (unrelated)
    given by kappa-s
-   Exclusion Counts number of exclusions for PM sample
-   Pairwise LR comparing pm sample in MP position to unrelated
-   Joint. Also called `global`
-   Posterior. For each pm sample with flat prior

## Settings

Data summary and plot does not requiring relabeling of data. However,
other functionality does when input is a Familias file. The reason is
that all missing persons have the same name in the fam file.

## No of missing…

If any reference family contains more than one missing, the total number
of missing must be given here. Also, in this case the missing persons
should be named M1, …, Mn in the Familias file.

## Download DVI table output

The indicated output is downloaded to a csv file called tab.csv.
