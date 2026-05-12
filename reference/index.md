# Package index

## Processing model output

Processing output from an atlantis run

- [`process_atl_output()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/process_atl_output.md)
  : Creates post-processing output for Atlantis
- [`process_catch_fleet()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/process_catch_fleet.md)
  : Creates post-processing Catch output for Atlantis
- [`get_atl_paramfiles()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_atl_paramfiles.md)
  : Creates a dataframe of atlantis parameter files based on standard
  output
- [`get_forcing_temperature()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_forcing_temperature.md)
  : Read in temperature forcing by box or from output.nc
- [`get_habitable_boxes()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_habitable_boxes.md)
  : Returns a list of habitable boxes for atlantis groups based on
  temperature forcing
- [`get_param_FXXX_SY()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_param_FXXX_SY.md)
  : Get adult distribution (FXXX_SY) values from a biology prm file
- [`get_param_move_temp()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_param_move_temp.md)
  : Get min_move_temp & max_move_temp values from a biology prm file
- [`get_param_recruit_hdistrib()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_param_recruit_hdistrib.md)
  : Get horizontal distribution recruitment values from a biology prm
  file
- [`get_param_recruit_vdistrib()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_param_recruit_vdistrib.md)
  : Get vertical distribution recruitment values from a biology prm file
- [`get_param_vert()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_param_vert.md)
  : Get adult distribution (vertDAY_XXX, vertNIGHT_XXX) values from a
  biology prm file
- [`get_run_prm()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/get_run_prm.md)
  : Get parameters from the run prm file

## Plotting model output

Plotting the output from an atlantis run

- [`make_atlantis_diagnostic_figures()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/make_atlantis_diagnostic_figures.md)
  : Creates standard diagnostic figures for Atlantis output

## Diagnostic checks

Determining if the model is behaving as expected

- [`diag_cohortBiomass()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_cohortBiomass.md)
  : Test for distribution of biomass over age class
- [`diag_combine()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_combine.md)
  : Combine all diagnostic output
- [`diag_fleet_catch()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_fleet_catch.md)
  : Test fleet catch
- [`diag_footprints()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_footprints.md)
  : Compares the redistribution footprints for groups in the model
- [`diag_maxsize()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_maxsize.md)
  : Test for max fish size
- [`diag_persistence()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_persistence.md)
  : Test for functional group persistence
- [`diag_reasonability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_reasonability.md)
  : Test for functional group reasonability
- [`diag_stability()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_stability.md)
  : Test for functional group stability
- [`diag_temp_thresholds()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/diag_temp_thresholds.md)
  : Compares the temperature forcing data to the Group temperature
  thresholds

## Miscellaneous

Miscellaneous functions

- [`compare_spatial_vars()`](https://noaa-edab.github.io/atlantisdiagnostics/reference/compare_spatial_vars.md)
  : Creates post-processing output for Atlantis
- [`atlantisdiagnostics`](https://noaa-edab.github.io/atlantisdiagnostics/reference/atlantisdiagnostics-package.md)
  [`atlantisdiagnostics-package`](https://noaa-edab.github.io/atlantisdiagnostics/reference/atlantisdiagnostics-package.md)
  : atlantisdiagnostics: Tools for calibrating Atlantis ecosystem models
