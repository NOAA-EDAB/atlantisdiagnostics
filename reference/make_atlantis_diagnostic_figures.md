# Creates standard diagnostic figures for Atlantis output

Used after the post processing routine
[`process_atl_output`](https://noaa-edab.github.io/atlantisdiagnostics/reference/process_atl_output.md).
Creates the full set of diagnostic and summary figures and tables from
atlantis model run.

## Usage

``` r
make_atlantis_diagnostic_figures(
  out.dir,
  fig.dir,
  atl.dir,
  param.dir,
  run.prefix,
  run.name,
  param.ls,
  benthic.box,
  benthic.level = 4,
  plot.all,
  plot.benthic,
  plot.overall.biomass,
  plot.biomass.timeseries,
  plot.length.age,
  plot.biomass.box,
  plot.c.mum,
  plot.sn.rn,
  plot.recruits,
  plot.numbers.timeseries,
  plot.physics,
  plot.growth.cons,
  plot.cohort,
  plot.diet,
  plot.consumption,
  plot.spatial.biomass,
  plot.spatial.biomass.seasonal,
  plot.catch,
  plot.spatial.catch,
  plot.weight,
  plot.mortality
)
```

## Arguments

- out.dir:

  string. Path where desired output data should be saved

- fig.dir:

  string. Path where desired output figures should be saved

- atl.dir:

  string. Path where atlantis model output is located

- param.dir:

  string. Path where atlantis parameter files are located

- run.prefix:

  sring. Prefix specified in Atlantis.bat file that begins each output
  file

- run.name:

  string. Prefered name to use for output file names

- param.ls:

  list. Output from get_atl_paramfiles

- benthic.box:

  numeric. Box ID for benthic plots

- benthic.level:

  numeric. Level for benthic plots (default is 4 for NEUS model)

- plot.all:

  Boolean. Global flag for plotting everything

- plot.benthic:

  logical. Benthic plots show timeseries of all benthic and epibenthic
  groups for one box

- plot.overall.biomass:

  logical. Plots showing the total biomass across all functional groups
  as stacked barplots

- plot.biomass.timeseries:

  logical. Plots showing biomass-related timeseries on various
  aggregations and reference points

- plot.length.age:

  logical. Plots relating to the length-age relationship of
  age-structured groups

- plot.biomass.box:

  logical. Plots relating to biomass but grouped by box

- plot.c.mum:

  logical. Plots and tables related to tuning C and mum parameters

- plot.sn.rn:

  logical. Plots relating to SN and RN timeseries

- plot.recruits:

  logical. Plots of recruitment and SSB timeseries

- plot.numbers.timeseries:

  logical. Plots showing timeseries of numbers (as opposed to biomass)

- plot.physics:

  logical. Plots of physical statevariables as well as fluxes

- plot.growth.cons:

  logical. Plots relating to growth and consumption

- plot.cohort:

  logical. Plots showing timeseries of each cohort across age-structured
  groups

- plot.diet:

  logical. Plots showing predation of and consumption by each functional
  group

- plot.consumption:

  Boolean. Plots showing consumption

- plot.spatial.biomass:

  logical. Plots showing the spatial (box/level) structure of groups'
  biomass

- plot.spatial.biomass.seasonal:

  logical. Plots showing the spatial (box/level) structure of groups'
  biomass

- plot.catch:

  logical. Plots annual catch(mt) age based catch (numbers) and age
  based %ages

- plot.spatial.catch:

  logical. Plots showing the spatial (box/level) structure of groups'
  catch

- plot.weight:

  logical. Plots the maximum size of fish in each size class over the
  domain

- plot.mortality:

  logical. Plots Mortality (F, M1, M2) from two output sources (Mort,
  SpecificMort)

## Value

A series of figures and tables based on output grouping flags

Author: Ryan Morse, modified by J. Caracappa
