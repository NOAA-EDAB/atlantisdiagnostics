# Creates post-processing output for Atlantis

Creates post-processing output for Atlantis

## Usage

``` r
process_atl_output(
  param.dir,
  atl.dir,
  out.dir = file.path(atl.dir, "Post_Processed/Data/"),
  run.prefix,
  param.ls,
  agg.scale = "day",
  large.file = F,
  system,
  process.all = F,
  plot.all = F,
  plot.benthic = F,
  plot.overall.biomass = F,
  plot.biomass.timeseries = F,
  plot.length.age = F,
  plot.biomass.box = F,
  plot.c.mum = F,
  plot.sn.rn = F,
  plot.recruits = F,
  plot.numbers.timeseries = F,
  plot.physics = F,
  plot.growth.cons = F,
  plot.cohort = F,
  plot.diet = F,
  plot.consumption = F,
  plot.spatial.biomass = F,
  plot.spatial.biomass.seasonal = F,
  plot.catch = F,
  plot.catch.fleet = F,
  plot.spatial.catch = F,
  plot.mortality = F,
  plot.weight = F,
  plot.spatial.overlap = F
)
```

## Arguments

- param.dir:

  string. Path to location of atlantis parameter files

- atl.dir:

  string. path to location of atlantis output files

- out.dir:

  string. path to desired location of post-processed output

- run.prefix:

  string. Prefix for atlantis run output (specified in runcommand.bat)

- param.ls:

  list generated from get_atl_paramfiles()

- agg.scale:

  Scale to aggregate dietcheck biomass from (either 'raw','month', or
  'year' )

- large.file:

  Boolean.

- system:

  String. "Windows" or "Linux"

- process.all:

  Boolean. Global option to process all components

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

- plot.catch.fleet:

  Boolean

- plot.spatial.catch:

  logical. Plots showing the spatial (box/level) structure of groups'
  catch

- plot.mortality:

  logical. Plots Mortality (F, M1, M2) from two output sources (Mort,
  SpecificMort)

- plot.weight:

  logical. Plots the maximum size of fish in each size class over the
  domain

- plot.spatial.overlap:

  Boolean

## Value

Either saves an R object or returns a list called "result"

Author: Ryan Morse, modified by Joseph Caracappa
