# Test for functional group reasonability

Inspects each time point. If at any point in time a species falls below
or rises above predefined bounds it is flagged. The term "reasonable" is
based on survey data when available.

## Usage

``` r
diag_reasonability(
  fgs,
  biomind,
  initialYr = 1964,
  startYr = 1998,
  speciesCodes = NULL,
  realBiomass,
  useVariance = F,
  nYrs = NULL,
  surveyBounds = c(1, 1),
  initBioBounds = c(0.5, 10)
)
```

## Arguments

- fgs:

  A character string. Path to location of functional groups file.

- biomind:

  A character string. Path to the BiomIndx.txt file.

- initialYr:

  Numeric Scalar. Year in which the model run was initiated. (Default =
  1964)

- startYr:

  Numeric Scalar. Year in which the model finishes spin up period.
  (Default = 1998)

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for reasonableness (Default = NULL, uses all species found in
  `modelBiomass`. Species codes should be a subset of the Atlantis
  species codes

- realBiomass:

  A data frame. biomass time series (from assessments, stock SMART or
  otherwise) for species. `realBiomass` should be in long format with
  column labels (YEAR,variable,value,Code,Species,isFishedSpecies)
  variable should contain value = "biomass" (Biomass units should be in
  metric tonnes) and/or "var" if `useVariance = T`

- useVariance:

  Boolean. If to use variance estimates of biomass (included in
  `realBiomass`) (Default = F, reverts to using `surveyBounds` as upper
  and lover bounds)

- nYrs:

  Numeric scalar. Number of years from the end of the time series for
  which reasonableness is checked. (Default = NULL, contemporary period
  from startYr is used)

- surveyBounds:

  Numeric vector. Size of 1x2 containing the values in which to multiple
  lower and upper bounds of observed data. For example (Default =
  c(1,1)) indicating use of min and max of observed biomass

- initBioBounds:

  Numeric vector. Size of 1x2 containing lower and upper bound
  proportions used to scale initial biomass. This is used for
  groups/species that dont have surveys

## Value

Returns a data frame of species

- species:

  The common name of the species/functional group as described in
  Atlantis input file

- code:

  Atlantis Code for species/functional group

- initialBiomass:

  Starting value of Biomass for species/functional group. From model
  output

- minBiomass:

  The smallest value of biomass observed in the run

- maxBiomass:

  The largest value of biomass observed in the run

- propInitiBio:

  The maxBiomass as a proportion of the inintial biomass

- propBelowLower:

  Magnitude of lower bound excedence as a proportion of lower threshold

- propAboveUpper:

  Magnitude of upper bound excedence as a proportion of upper threshold

- maxExceedance:

  max of `propBelowLower` and `propAboveUpper`

- t1:

  The first year reasonableness was not met

- tn:

  The last year reasonableness was not met

- nts:

  The total number of years that reasonableness was not met

- modelSkill:

  Measure of the goodness of fit

- nObs:

  number of observations used in the goodness of fit

- pass:

  Boolean. Indicates if the Atlantis group passed reasonability

- test:

  Character. Indicate how `pass` was determined. If the Atlantis species
  is fished and the species is a stand alone species in the model then
  survey data is used to assess reasonability. Otherwise deviations from
  initial biomass is used

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Declare paths to files required

biomind <- paste("Full path to file","xxxBiomIndx.txt")
fgs <- paste("Full path to file","functionalGroups.csv")

# read in survey biomass and convert to metric tons
realBiomass <- readRDS(paste0(dataDir,"sweptAreaBiomassNEUS.rds")) %>%
      dplyr::filter(variable %in% c("tot.biomass")) %>%
      dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
      dplyr::select(-units)

# Perform reasonability test on all species/groups using the last 20 years of the run.
# Allow species with data to be bounded by 100 x max observed biomass and 1 x min observed biomass.
# For species without data allow model biomass to lie between 0.5 and 2 times initial biomass

diag_reasonability(fgs, biomind, initialYr = 1964, realBiomass=realBiomass,
surveyBounds = c(1,100), initBioBounds = c(0.5,2))

# Only perform test on herring and white hake.
diag_reasonability(fgs, biomind, initialYr = 1964, speciesCodes =c("MAK","WHK"),
 realBiomass=realBiomass, surveyBounds = c(1,100), initBioBounds = c(0.5,2))
} # }
```
