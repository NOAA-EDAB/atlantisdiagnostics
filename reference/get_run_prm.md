# Get parameters from the run prm file

Pull out the parameters from the run prm file. This will only return the
values of parameters that have a scalar value. For example, if paramName
= init_scalar, only the length of the vector will be returned.

## Usage

``` r
get_run_prm(runprm, paramName = NULL)
```

## Arguments

- runprm:

  path to the run prm file

- paramName:

  name of the parameter to extract

## Value

list

- value:

  value of parameter

- unit:

  units of parameter, if applicable
