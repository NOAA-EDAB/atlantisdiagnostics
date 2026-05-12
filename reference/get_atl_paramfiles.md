# Creates a dataframe of atlantis parameter files based on standard output

Creates a dataframe of atlantis parameter files based on standard output

## Usage

``` r
get_atl_paramfiles(param.dir, atl.dir, run.prefix, include_catch)
```

## Arguments

- param.dir:

  string. Path to location of atlantis parameter files

- atl.dir:

  string. Path to location of atlantis output files

- run.prefix:

  string. Name of the run. filename prefix in the atlantis output

- include_catch:

  logical. Whether to include catch output

## Value

list containing parameter file name and full path to file
