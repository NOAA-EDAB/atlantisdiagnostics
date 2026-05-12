# Returns a list of habitable boxes for atlantis groups based on temperature forcing

Determine if Groups have suitable habitat (set of polygons/boxes by
layer) based on temperature thresholds.

Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are NOT
used.

## Usage

``` r
get_habitable_boxes(paramList, speciesCodes = NULL, timeFrame = -1)
```

## Arguments

- paramList:

  A list of parameter files (Output of
  [`get_atl_paramfiles()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_atl_paramfiles.md))

- speciesCodes:

  A character string of the species/group name of interest. Default is
  NULL (All species)

- timeFrame:

  Numeric. Section of time series of interest. For example, 10:20
  (between year 10 and 20), -1 (last year of the timeseries), -10 (last
  10 years of the time series)

## Value

A data frame. Box/layers which are habitable over time range. The
columns are:

- group:

  Species/Group name

- layer:

  Polygon layer

- habitableBoxes:

  The Boxes/polygons in which are habitable based on temperature
  thresholds

## Layers

1 = Surface, n is sediment

## Examples

``` r
if (FALSE) { # \dontrun{
# Declare paths to files required
paramList <- list()
paramList$bgm.file <- "Full path to bgm file"
paramList$biol.prm <- "Full path to biology prm file"
paramList$run.prm <- "Full path to run prm file"

# Get all habitable boxes in the last year of the run for all species
get_habitable_boxes(paramList, speciesCodes=NULL, timeFrame = -1)

# Get all habitable boxes for HERRING and WHITE HAKE for the period of the last 10 years
# Note: if a box is considered habitable for any of the last 10 years it will be present in the output
diag_temp_thresholds(paramList, speciesCodes=c("HER","WHK"), timeFrame = -10)

# Get all habitable boxes for HERRING between the 10th and 11th year of the temperature forcing data
diag_temp_thresholds(paramList, speciesCodes=c("HER"), timeFrame = 10:11)
} # }

```
