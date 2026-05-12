# Compares the redistribution footprints for groups in the model

Determine if Groups have the sam footprint for timestep redistribution
Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are used to
compare the spatial extent of the groups.

## Usage

``` r
diag_footprints(paramList, speciesCodes = NULL)
```

## Arguments

- paramList:

  A list of parameter files (Output of
  [`get_atl_paramfiles()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_atl_paramfiles.md))

- speciesCodes:

  A character string of the species/group name of interest. Default is
  NULL (All species)

## Value

A data frame. The columns are:

- group:

  Species/Group name

- adultJuv:

  Logical value indicating if the adult and juvenile spatial
  distributions are the same

- recruitJuv:

  Logival value indicating if the juvenile and recruit spatial
  distributions are the same

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Declare paths to files required
paramList <- list()
paramList$bgm.file <- "Full path to bgm file"
paramList$biol.prm <- "Full path to biology prm file"
paramList$run.prm <- "Full path to run prm file"
paramList$groups.file <- "Full path to group csv file"
# check for any species that maybe impacted by temperature
diag_footprints(paramList,speciesCodes=NULL)

# check for HERRING and WHITE HAKE
diag_footprints(paramList,speciesCodes=c("HER","WHK"))
} # }
```
