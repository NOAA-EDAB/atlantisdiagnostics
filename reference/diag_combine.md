# Combine all diagnostic output

Using the output from all tests (persistence, stability, reasonability),
they are combined into a single data frame

## Usage

``` r
diag_combine(persistence, stability, reasonability)
```

## Arguments

- persistence:

  Tibble. Output from `diag_persistence`

- stability:

  Tibble. Output from `diag_stability`

- reasonability:

  Tibble. Output from `diag_reasonability`

## Value

A data frame

- species:

  The common name of the species/functional group as described in
  Atlantis input file

- code:

  Atlantis Code for species/functional group

- persistence:

  Indicating pass or failure of persistence test

- proportionInitBio:

  Metric for persistence. The proportion of initial biomass at
  `tminimumBiomass`

- stability:

  Indicating pass or failure of stability test

- relChange:

  Metric for stability. Rate of increase relative to
  `t1Biomass`(`mtperyear`/`t1Biomass`). see
  [`diag_stability`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_stability.md)

- reasonability:

  Indicating pass or failure of reasonability test
