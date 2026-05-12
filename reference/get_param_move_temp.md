# Get min_move_temp & max_move_temp values from a biology prm file

The values are the minimum and maximum temperatures that a group can
tolerate XXX_min_move_temp, XXX_max_move_temp

## Usage

``` r
get_param_move_temp(bio.prm)
```

## Arguments

- bio.prm:

  path to the biology prm file

## Value

a data frame with the group, value and limit (max/min)
