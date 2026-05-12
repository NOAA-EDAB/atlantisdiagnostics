# Test for distribution of biomass over age class

Determine which age classes contain the most biomass and compare to
neighboring age classes. A species fails the test if all biomass is
contained in either the smallest or largest age class

## Usage

``` r
diag_cohortBiomass(
  fgs,
  mortality,
  agebiomind,
  speciesCodes = NULL,
  neusPriority,
  fishedSpecies = 2
)
```

## Arguments

- fgs:

  A character string. Path to location of functional groups file.

- mortality:

  A character string. Path to location of Mort.txt file.

- agebiomind:

  A character string. Path to location of AgeBiomIndx file.

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for persistence. (Default = NULL, uses all species with `IsTurnedOn=1`
  in `fgs` file)

- neusPriority:

  A character string. Path to location of Species Priorities file.

- fishedSpecies:

  Numeric scalar. Filter by how much species are fished. Values = 1
  (most), 2, 3, 4 (least) (Default = 2. Only species fished at level \<=
  2 will be returned)

## Value

- code:

  Atlantis species code

- status:

  Boolean indicating whether species passes test

- maxCohort:

  Age class with the highes biomass

- stability:

- priority:

  Scale defining priority of species in the model, High (H), Low(L)

- fishing:

  Scale defining level of fishing, 1 is highest

## See also

Other diagnostics:
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)
