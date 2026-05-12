# Get vertical distribution recruitment values from a biology prm file

The values are proportion of recruits and how they are distributed among
the polygon layers. Layer 1 = surface, 4 = deepest. Note: Sediment layer
is absent. XXX_recruit_vdistrib

## Usage

``` r
get_param_recruit_vdistrib(bio.prm)
```

## Arguments

- bio.prm:

  path to the biology prm file

## Value

a data frame with the group, value and layer
