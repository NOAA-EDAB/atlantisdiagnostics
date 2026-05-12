# Creates post-processing Catch output for Atlantis

Reads in catch output from catch.nc file by species, fleet, box, time

## Usage

``` r
process_catch_fleet(fishery.prm, catch, groups.file)
```

## Arguments

- fishery.prm:

  string. Path to location of atlantis fishery csv file

- catch:

  string. path to location of atlantis catch output nc file

- groups.file:

  string. path to location of atlantis functional groups csv file

## Value

returns a dataframe

- species:

  long species name

- fleet:

  fleet name

- polygon:

  atlantis box/polygon number

- time:

  time step

- atoutput:

  metric tons

Author: Joseph Caracappa
