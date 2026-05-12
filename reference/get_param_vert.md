# Get adult distribution (vertDAY_XXX, vertNIGHT_XXX) values from a biology prm file

Get the values (proportion of the population) that define the
distribution among layers of a boxes for each 12 hour period.
vertDAY_XXX and vertNIGHT_XXX are vectors. The length is the number of
non sediment layers. The first entry for each vector refers to water
column layer closest to the sediment and then up through the water
column. Sediment layer is absent

## Usage

``` r
get_param_vert(bio.prm)
```

## Arguments

- bio.prm:

  path to the biology prm file

## Value

a data frame

- group:

  Character string. Atlantis group code

- daynight:

  Character string. "day" or "night"

- cohort:

  Character string. "adult", "juvenile" or "biomass"

- layer:

  Integer. Layer number

- value:

  Value. Proportion of the population in the layer
