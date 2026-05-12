# Test for max fish size

Calculate average size of fish through time and check to see if any fish
exceed the user supplied observed maximum size. Both maximum weight and
maximum length are used

## Usage

``` r
diag_maxsize(
  nc,
  bgm,
  init,
  fgs,
  prm_run,
  prm_biol,
  speciesStats,
  speciesCodes = NULL
)
```

## Arguments

- nc:

  A character string. Path to location of main nc file.

- bgm:

  A character string. Path to location of box coordinates bgm file.

- init:

  A character string. Path to location of initial conditions nc file.

- fgs:

  A character string. Path to location of functional groups file.

- prm_run:

  A character string. Path to location of run parameter file file.

- prm_biol:

  A character string. Path to location of biology parameter file.

- speciesStats:

  Data frame. Must contain at least 3 columns labeled `code` - Atlantis
  species codes ,`maxObsWeight` - a value (g) indicating the maximum
  weight the species should weigh, `maxObsLength` - a value (cm)
  indicating the maximum length the species should grow to.

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for large fish. (Default = NULL, uses all species)

## Value

Returns a data frame indicating which species meet defined max
weight/length criterion.

- species:

  The common name of the species/functional group

- code:

  Atlantis Code for species/functional group

- polygon:

  Polygon in which the largest individual occupies

- agecl:

  Age class of the largest individual

- time:

  Time in which the largest individual was found (Decimal years)

- maxLength:

  Maximum length of species in Atlantis (cm)

- maxObsLength:

  Maximum observed length of species (cm) - from litereature

- maxMeanWeight:

  Maximum mean weight of indivdual in Atlantis (g)

- maxObsWeight:

  Maximum observed weight of species (g) - from litereature

- passW:

  Boolean indicating whether species maximum weight (from Atlantis)
  falls below max observed weight

- ratioW:

  ratio of atlantis max weight to observed max weight

- passL:

  Boolean indicating whether species maximum length (from Atlantis)
  falls below max observed length

- ratioL:

  ratio of atlantis max length to observed max length

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)
