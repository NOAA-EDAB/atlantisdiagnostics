# Test for functional group stability

`diag_stability` determines whether the model reaches a steady state
over the last n years of a run. Stability is loosely defined as a
species/groups biomass reaching a stable level (measured as having
tolerable trend in a defined time range).

## Usage

``` r
diag_stability(
  fgs,
  biomind,
  initialYr = 1964,
  speciesCodes,
  nYrs = 20,
  relChangeThreshold = 0.01
)
```

## Arguments

- fgs:

  A character string. Path to location of functional groups file.

- biomind:

  A character string. Path to the BiomIndx.txt file.

- initialYr:

  Numeric Scalar. Year in which the model run was initiated. (Default =
  1964)

- speciesCodes:

  Character vector. A vector of Atlantis species codes in which to test
  for stability. (Default = NULL, uses all species found in
  `modelBiomass`)

- nYrs:

  Numeric scalar. Number of years from the end of the time series that
  stability must occur. (Default = 20 years)

- relChangeThreshold:

  Numeric Scalar. Maximum magnitude of relative change of slope (Default
  = 0.01)

## Value

Returns a data frame of all species and how they measure up against the
stability criterion

- code:

  Atlantis Code for species/functional group

- species:

  The common name of the species/functional group

- t1Fit:

  Value of fitted biomass for the first of year data used in the fit

- mtPerYear:

  Double. The value of the slope parameter (year)

- relChange:

  Rate of increase relative to `t1Fit`(`mtPerYear`/`t1Fit`)

- aveBio:

  mean biomass for the last `nYrs` years

- pass:

  Logical. Does the species/group pass the test for stability

## Details

Formally the following model is fit to the last n years of the run:

\$\$biomass_t = \mu + \beta.t + \epsilon_t where \epsilon_t ~ IID
N(0,\sigma^2)\$\$

where null hypothesis, \$\$H0:\beta=0\$\$

Note: annual biomass is used in fitting. Species with mean annual
biomass \< 1 metric ton over the last n years of the run are not
considered stable. They are reported to Fail the test and NaNs returned

## Examples

``` r
if (FALSE) { # \dontrun{
# Declare paths to files required

biomind <- paste("Full path to file","xxxBiomIndx.txt")
fgs <- paste("Full path to file","functioalGgroups.csv")

# Perform stability test on all species/groups using the last 20 years of the run
diag_stability(fgs, biomind, nYrs = 20)

# Only perform test on herring and white hake.
# Require stability over the last 10 years of the run and and use a
# relative change in the slope = 0.01 as the criterion for pass or fail
diag_stability(fgs,biomind, speciesCodes=c("HER","WHK"), nYrs = 10, relChangeThreshold = 0.01)
} # }
```
