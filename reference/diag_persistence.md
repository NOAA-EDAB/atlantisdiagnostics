# Test for functional group persistence

Inspects each time point. If at any point in time a species falls below
a predefined floor it is flagged. The floors are specifies as a
proportion of initial biomass. The last n years of the run are used in
the test

## Usage

``` r
diag_persistence(
  fgs,
  biomind,
  speciesCodes = NULL,
  nYrs = NULL,
  floor = 0.1,
  display = NULL,
  tol = 1e-06
)
```

## Arguments

- fgs:

  A character string. Path to location of functional groups file.

- biomind:

  A character string. Path to the BiomIndx.txt file.

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for persistence. (Default = NULL, uses all species with `IsTurnedOn=1`
  in `fgs` file)

- nYrs:

  Numeric scalar. Number of years from the end of the time series that
  persistence must occur. (Default = NULL, persistence must occur
  throughout entire time series)

- floor:

  Numeric scalar. Proportion of initial biomass for which for which all
  species are measured against. (Default = 0.1, all species need to be
  above 10% of initial biomass). Range should be 0-1

- display:

  Boolean. Flag to indicate whether to return only species that pass the
  test, fail the test, or all species (Default = NULL)

- tol:

  Numeric scalar. Tolerance level to add to biomass floor (Default =
  1E-6)

## Value

Returns a data frame of species which do not meet defined persistence
criteria.

- species:

  The common name of the species/functional group

- code:

  Atlantis Code for species/functional group

- initialBiomass:

  Starting value of Biomass for species/functional group

- minimumBiomass:

  The smallest value of biomass observed in the run

- tminimumBiomass:

  The time step in which `minimumBiomass` occurred

- proportionInitBio :

  The proportion of initial biomass at `tminimumBiomass`

- t1:

  The first time step persistence was not met

- tn:

  The last time step persistence was not met

- nts:

  The total number of time steps that persistence was not met

- pass:

  Boolean. Indicates if the Atlantis group passed persistence test

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Declare paths to files required
biomind <- paste("Full path to file","xxxBiomIndx.txt")
fgs <- paste("Full path to file","functioalGgroups.csv")

# find all species that do not have biomass > 0 for any time during the run.
diag_persistence(fgs,biomind,speciesCodes=NULL, nYrs = NULL, floor = 0)

# only evaluate herring. Require stability over the last 10 years of the run and all values should
# exceed 10% of initial biomass
diag_persistence(fgs,biomind, speciesCodes="HER", nYrs = 10, floor = 0.1)

} # }
```
