# Test fleet catch

`diag_fleet_catch` determines whether the simulated fleets catcht the
right magnitude and spatial distribution of catch over the last n years
of a run.

## Usage

``` r
diag_fleet_catch(
  fgs,
  fishery.prm,
  catch.file,
  bgm,
  catch.ref,
  speciesCodes = NULL,
  nYrs = 20,
  min.dist = 100,
  relChangeThreshold = 0.01
)
```

## Arguments

- fgs:

  A character string. Path to location of functional groups file.

- fishery.prm:

  A character string. Path to the fishery definitions file.

- catch.file:

  A character string. Path to catch nc file

- bgm:

  A character string. Path to the bgm file.

- catch.ref:

  A data.frame containing reference catch by fleet data
  (species\|fleet\|polygon\|ref.value)

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for stability.

- nYrs:

  Numeric scalar. Number of years from the end of the time series that
  stability must occur.

- min.dist:

  Numeric Scalar. Maximum distance between model and reference center of
  gravity

- relChangeThreshold:

  Numeric Scalar. Maximum magnitude of relative change of slope (Default
  = 0.01)

## Value

Returns a data frame of all species and how they measure up against the
catch by fleet criteria

- species:

  The common name of the species/functional group

- fleet:

  The fleet name from the fisheries file

- catch.model:

  Double. The mean catch in mT over the last nYrs of the model

- catch.ref:

  Double. The reference catch (mT)

- cog.x.model:

  Double. The mean x-coordinate center of gravity from the model

- cog.x.ref:

  Double. The mean x-coordinate center of gravity from the reference

- cog.y.model:

  Double. The mean y-coordinate center of gravity from the model

- cog.y.ref:

  Double. The mean y-coordinate center of gravity from the reference

- dist:

  Double. The mean distance between the center of gravity between the
  model and reference

- catch.magnitude:

  Logical. Is the relative difference between model and refernece catch
  within +/- relChangeThreshold?

- catch.dist:

  Logical. Is the distance between center of gravity betwene model and
  reference less than min.dist?

## See also

Other diagnostics:
[`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md),
[`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md),
[`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md),
[`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md),
[`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md),
[`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)
