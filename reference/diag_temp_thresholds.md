# Compares the temperature forcing data to the Group temperature thresholds

Determine if Groups have suitable habitat based on temperature
thresholds. Horizontal redistribution proportions (FXXX_SY and
recruit_hdist) and vertical distributions (vertDAY,vertNIGHT,
recruit_vdistrib) are used to identify the spatial extent of the groups.
Groups are then assessed as to whether these polygons are considered
habitable based on temperature.

Note: The union of polygons over all seasons is used in comparisons

## Usage

``` r
diag_temp_thresholds(paramList, speciesCodes = NULL)
```

## Arguments

- paramList:

  A list of parameter files (Output of
  [`get_atl_paramfiles()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_atl_paramfiles.md))

- speciesCodes:

  A character string of the species/group name of interest. Default is
  NULL (All species)

## Value

A data frame. Only groups that fail are returned. The columns are:

- group:

  Species/Group name

- layer:

  Polygon layer

- recruitBoxes:

  Proportion of boxes that are not habitable for recruits due to
  temperature (relative to defined range)

- ageBoxesAdult:

  Proportion of boxes that are not habitable for adults due to
  temperature (relative to defined range)

- ageBoxesJuv:

  Proportion of boxes that are not habitable for juveniles due to
  temperature (relative to defined range)

- recruitTime:

  Proportion of time in model that recruits were distributed away from
  non habitable polygons

- ageTimeAdult:

  Proportion of time in model that adults were distributed away from non
  habitable polygons

- ageTimeJuv:

  Proportion of time in model that juveniles were distributed away from
  non habitable polygons

- pass:

  Logical indicating if the species passes the temperature threshold
  test. All fields \< 0.01

## Layers

1 = Surface, n is sediment The sediment layer is not returned in the
output.

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Declare paths to files required
paramList <- list()
paramList$bgm.file <- "Full path to bgm file"
paramList$biol.prm <- "Full path to biology prm file"
paramList$run.prm <- "Full path to run prm file"

# check for any species that maybe impacted by temperature
diag_temp_thresholds(paramList,speciesCodes=NULL)

# check for HERRING and WHITE HAKE
diag_temp_thresholds(paramList,speciesCodes=c("HER","WHK"))
} # }

```
