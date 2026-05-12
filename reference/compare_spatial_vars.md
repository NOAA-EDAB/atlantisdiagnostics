# Creates post-processing output for Atlantis

Creates post-processing output for Atlantis

## Usage

``` r
compare_spatial_vars(
  param.dir,
  run.dirs,
  run.names,
  ref.data,
  init.data,
  out.dir,
  out.name,
  param.ls,
  data.type = "proportion",
  comparison.type = "difference",
  ref.years,
  speciesCodes = NULL,
  plot = T
)
```

## Arguments

- param.dir:

  string. Path to location of atlantis parameter files

- run.dirs:

  character vector of paths to location of atlantis output files

- run.names:

  character vector of run names corresponding to run.dirs

- ref.data:

  dataframe that contains reference spatial data with format
  (polygon\|species\|var.name\|statistic\|ref.value)

- init.data:

  dataframe that contains initial spatial data with format
  (polygon\|species\|var.name\|statistic\|init.value)

- out.dir:

  string. path to desired location of post-processed output

- out.name:

  string. name for output file prefix

- param.ls:

  list generated from get_atl_paramfiles()

- data.type:

  which type of data is being compared: 'proportion','value'

- comparison.type:

  which type of comparison should be made: 'difference','scalar'

- ref.years:

  numeric vector with first and last year (years from start) for
  comparison period

- speciesCodes:

  character vector. List of species to make plots for. Defaul = NULL
  (All species)

- plot:

  logical. do you want to generate a plot

## Value

Either saves an R object or returns a list called "result"

Author: Joseph Caracappa
