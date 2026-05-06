#' Returns a list of habitable boxes for atlantis groups based on temperature forcing
#'
#'@description
#'Determine if Groups have suitable habitat (set of polygons/boxes by layer) based on temperature thresholds.
#'
#'Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
#'vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are NOT used.
#'
#'
#'@param paramList A list of parameter files (Output of \code{get_atl_paramfiles()})
#'@param speciesCodes A character string of the species/group name of interest. Default is NULL (All species)
#'@param timeFrame Numeric. Section of time series of interest. For example, 10:20 (between year 10 and 20), -1 (last year of the timeseries), -10 (last 10 years of the time series)
#'
#'@return A data frame. Box/layers which are habitable over time range. The columns are:
#'\item{group}{Species/Group name}
#'\item{layer}{Polygon layer}
#'\item{habitableBoxes}{The Boxes/polygons in which are habitable based on temperature thresholds}
#'
#' @section Layers:
#' 1 = Surface, n is sediment
#'
#'
#'@examples
#'\dontrun{
#'# Declare paths to files required
#' paramList <- list()
#' paramList$bgm.file <- "Full path to bgm file"
#' paramList$biol.prm <- "Full path to biology prm file"
#' paramList$run.prm <- "Full path to run prm file"
#'
#' # Get all habitable boxes in the last year of the run for all species
#' get_habitable_boxes(paramList, speciesCodes=NULL, timeFrame = -1)
#'
#' # Get all habitable boxes for HERRING and WHITE HAKE for the period of the last 10 years
#' # Note: if a box is considered habitable for any of the last 10 years it will be present in the output
#' diag_temp_thresholds(paramList, speciesCodes=c("HER","WHK"), timeFrame = -10)
#'
#' # Get all habitable boxes for HERRING between the 10th and 11th year of the temperature forcing data
#' diag_temp_thresholds(paramList, speciesCodes=c("HER"), timeFrame = 10:11)
#'}
#'
#'
#'@export

get_habitable_boxes <- function(
  paramList,
  speciesCodes = NULL,
  timeFrame = -1
) {
  # check to see if all parameter files are available

  check_param_files(paramList$run.prm)
  check_param_files(paramList$bgm.file)
  check_param_files(paramList$biol.prm)

  ## Grab the min and max temperature by polygon from the biology.prm file
  temperatureLimits <- get_param_move_temp(paramList$biol.prm)

  # check to see if species codes are valid
  if (is.null(speciesCodes)) {
    speciesCodes <- unique(temperatureLimits$group)
  } else {
    # check to see if speciesCodes is in the temperatureLimits
    if (any(!speciesCodes %in% unique(temperatureLimits$group))) {
      stop(paste0(
        "Some species Codes not found: ",
        paste0(speciesCodes, collapse = ",")
      ))
    }
  }

  # get the temperature forcing data by time/polygon/layer
  # increase layer value to match other outputs
  temperatureData <- get_forcing_temperature(paramList, plotFigs = F) |>
    dplyr::mutate(layer = as.numeric(levels(layer)[layer]) + 1)
  sedimentLayer <- max(temperatureData$layer)
  # read in the output frequency to scale the recruitment time diagnostic
  toutinc <- get_run_prm(paramList$run.prm, "toutinc")
  numValsPerYear <- 365 / toutinc$value
  # read in the time step
  dt <- get_run_prm(paramList$run.prm, "dt")$value

  # get boundary boxes
  boxcoords <- atlantistools::load_box(paramList$bgm.file)
  bboxes <- atlantistools::get_boundary(boxcoords)

  # extract values of time based on timeFrame argument passed
  maxModelTime <- max(temperatureData$time)
  if ((length(timeFrame) == 1) && timeFrame < 0) {
    # last so many years
    timeToFilter <- (maxModelTime - abs(timeFrame)):maxModelTime
  } else {
    timeToFilter <- timeFrame
  }

  ## Diagnostic to indicate which boxes in the model are habitable, based on temperature,
  # compared to the forcing time series.

  outdf <- NULL
  # loop over layers
  for (ilayer in sort(unique(temperatureData$layer))) {
    # use the forcing temp data temperatureData for each layer
    data <- temperatureData |>
      dplyr::filter(layer == ilayer) |>
      dplyr::select(-variable, -layer)
    if (nrow(data) == 0) {
      next
    }

    # loop over species
    # check to see if any species are affected by temperature
    # Determine if temperatureLimits fall inside the range of the temperature data
    for (species in speciesCodes) {
      # obtain the temperature thresholds for each species
      speciesData <- temperatureLimits |>
        dplyr::filter(group == species)
      # select the value of speciesData where limit = min
      # and the value of speciesData where limit = max
      minV <- speciesData |>
        dplyr::filter(limit == "min") |>
        dplyr::select(value) |>
        dplyr::pull()
      maxV <- speciesData |>
        dplyr::filter(limit == "max") |>
        dplyr::select(value) |>
        dplyr::pull()

      # check to see if the species thresholds fall within the temperature data
      # Habitable range based on time frame of interest

      habitableBoxes <- data |>
        dplyr::filter(
          atoutput > minV & atoutput < maxV,
          time > min(timeToFilter) & time <= max(timeToFilter)
        ) |>
        dplyr::pull(polygon) |>
        unique()

      # if there are no instances move on to next species
      if (length(habitableBoxes) <= 0) {
        next
      }

      outdflayer <- data.frame(
        group = species,
        layer = ilayer,
        habitableBoxes = habitableBoxes
      )

      outdf <- rbind(outdf, outdflayer)
    }
  }

  return(outdf)
}
