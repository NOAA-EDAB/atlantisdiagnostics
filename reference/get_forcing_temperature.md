# Read in temperature forcing by box or from output.nc

To get a better idea of how temperature changes in space and time

Important to discern if species distribution changes are due to changes
in temperature or something else or to identify which regions a species
might leave if temperature was increased under climate scenarios

## Usage

``` r
get_forcing_temperature(param.ls, plotFigs = F)
```

## Arguments

- param.ls:

  list. List of file paths from Atlantis model

- plotFigs:

  logical. If TRUE, plots are generated

## Value

tibble. Data frame of temperature data by box/layer/time

## Layers

0 = Surface, 4 is sediment
