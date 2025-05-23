#' Compares the temperature forcing data to the Group temperature thresholds
#'
#'@description
#'Determine if Groups have suitable habitat based on temperature thresholds.
#'Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
#'vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are used to
#'identify the spatial extent of the groups. Groups are then assessed as to whether
#'these polygons are considered habitable based on temperature.
#'
#'Note: The union of polygons over all seasons is used in comparisons
#'
#'@param paramList A list of parameter files (Output of \code{get_atl_paramfiles()})
#'@param speciesCodes A character string of the species/group name of interest. Default is NULL (All species)
#'
#'@return A data frame. Only groups that fail are returned. The columns are:
#'\item{group}{Species/Group name}
#'\item{layer}{Polygon layer}
#'\item{recruitBoxes}{Proportion of boxes that are not habitable for recruits due to temperature (relative to defined range)}
#'\item{ageBoxesAdult}{Proportion of boxes that are not habitable for adults due to temperature (relative to defined range)}
#'\item{ageBoxesJuv}{Proportion of boxes that are not habitable for juveniles due to temperature (relative to defined range)}
#'\item{recruitTime}{Proportion of time in model that recruits were distributed away from non habitable polygons}
#'\item{ageTimeAdult}{Proportion of time in model that adults were distributed away from non habitable polygons}
#'\item{ageTimeJuv}{Proportion of time in model that juveniles were distributed away from non habitable polygons}
#'\item{pass}{Logical indicating if the species passes the temperature threshold test. All fields < 0.01}
#'
#' @section Layers:
#' 1 = Surface, n is sediment
#' The sediment layer is not returned in the output.
#'
#'@family diagnostics
#'
#'@examples
#'\dontrun{
#'# Declare paths to files required
#' paramList <- list()
#' paramList$bgm.file <- "Full path to bgm file"
#' paramList$biol.prm <- "Full path to biology prm file"
#' paramList$run.prm <- "Full path to run prm file"
#'
#' # check for any species that maybe impacted by temperature
#' diag_temp_thresholds(paramList,speciesCodes=NULL)
#'
#' # check for HERRING and WHITE HAKE
#' diag_temp_thresholds(paramList,speciesCodes=c("HER","WHK"))
#'}
#'
#'
#'@export

diag_temp_thresholds <- function(paramList, speciesCodes=NULL) {

  # check to see if all parameter files are available

  check_param_files(paramList$run.prm)
  check_param_files(paramList$bgm.file)
  check_param_files(paramList$biol.prm)


  ## Grab the min and max temperature by polygon from the biology.prm file
  temperatureLimits <- get_param_move_temp(paramList$biol.prm)
  if(is.null(speciesCodes)) {
    speciesCodes <- unique(temperatureLimits$group)
  } else {
    # check to see if speciesCodes is in the temperatureLimits
    if (!speciesCodes %in% unique(temperatureLimits$group)) {
      stop(paste0("Species Code", speciesCodes, " not found"))
    }
  }

  # get the temperature forcing data by time/polygon/layer
  # increase layer vlaue to match other outputs
  temperatureData <- get_forcing_temperature(paramList, plotFigs=F) |>
    dplyr::mutate(layer = as.numeric(levels(layer)[layer])+1)
  sedimentLayer <- max(temperatureData$layer)
  # read in the output frequency to scale the recruitment time diagnostic
  toutinc <- get_run_prm(paramList$run.prm, "toutinc")
  numValsPerYear <- 365/toutinc$value
  # read in the time step
  dt <- get_run_prm(paramList$run.prm, "dt")$value

  # get boundary boxes
  boxcoords <- atlantistools::load_box(paramList$bgm.file)
  bboxes <- atlantistools::get_boundary(boxcoords)

  # grab the horizontal redistrbution proportions for recruitment
  hdistRecruit <- get_param_recruit_hdistrib(paramList$biol.prm) |>
    dplyr::filter(!(polygon %in% bboxes),
                  value > 1e-5)
  # grab the vertical redistrbution proportions for recruitment
  vdistRecruit <- get_param_recruit_vdistrib(paramList$biol.prm) |>
    dplyr::filter(value > 1e-5)

  #get the horizontal redistribution proportions for non recruits
  hdistAge <- get_param_FXXX_SY(paramList$biol.prm)|>
    dplyr::filter(!(polygon %in% bboxes),
                  value > 1e-5)

  vdistAge <- get_param_vert(paramList$biol.prm)|>
    dplyr::filter(value > 1e-5)

  # use either day of night vertical distribution parameters if time step is 24 hours
  # otherwise use both day and night
  if (dt == 24) {
    vdistAge <- vdistAge |>
      dplyr::filter(daynight == "day") |>
      dplyr::select(-daynight)
  } else {
    stop("Not coded for dt != 24 hours")
  }

  ## diagnostic to indicate which species have temperature values
  # within the bounds of the forcing time series. This would indicate that
  # the species is forced to move.

  outdf <- NULL
  # loop over layers
  for (ilayer in sort(unique(temperatureData$layer))) {
    # use the forcing temp data temperatureData for each layer
    data <- temperatureData |>
      dplyr::filter(layer == ilayer) |>
      dplyr::select(-variable,-layer)
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

      # check to see if the species thresholds fall outside the temperature data
      extreme <- data |>
        dplyr::filter(atoutput < minV | atoutput > maxV)
      # if there are no instances move on to next species
      if (nrow(extreme) <= 0) {
        next
      }

      # check to see if the species recruits into current layer
      recruitLayers <- vdistRecruit |>
        dplyr::filter(group == species) |>
        dplyr::pull(layer)

      if (ilayer %in% recruitLayers) {
        # look for how temperature effects recruitment
        # select the polygons that the recruits are distributed to
        spatialExtentOfSpeciesRecruit <- hdistRecruit |>
          dplyr::filter(group == species) |>
          dplyr::select(polygon) |>
          dplyr::distinct() |>
          dplyr::pull()

        # filter all the polygons that recruits are distributed to that are outside of the species
        # thermal limits (not habitable)
        extremeD <- extreme |>
          dplyr::filter(polygon %in% spatialExtentOfSpeciesRecruit)

        # find proportion of boxes that are not habitable due to temperature
        propBoxesRecruits <- length(unique(extremeD$polygon)) / length(unique(spatialExtentOfSpeciesRecruit))
        # find proportion of time in which recruits are distributed to uninhabitable boxes
        propTimeRecruits <- length(unique(extremeD$time)) / (length(unique(data$time))/numValsPerYear)

      } else {
        # if the species does not recruit into the current layer
        # set the proportion of boxes and time to 0
        propBoxesRecruits <- 0
        propTimeRecruits <- 0
      }


      # check to see if the age structured species distributed into current layer
      ageLayers <- vdistAge |>
        dplyr::filter(group == species) |>
        dplyr::pull(layer)

      if (ilayer %in% ageLayers) {

        # Similar for Adults and Juveniles
        spatialExtentOfSpeciesAgeAdult <- hdistAge |>
          dplyr::filter(group == species,
                        cohort == "adult") |>
          dplyr::select(polygon) |>
          dplyr::distinct() |>
          dplyr::pull()
        spatialExtentOfSpeciesAgeJuv <- hdistAge |>
          dplyr::filter(group == species,
                        cohort == "juv") |>
          dplyr::select(polygon) |>
          dplyr::distinct() |>
          dplyr::pull()

        extremeDAdult <- extreme |>
          dplyr::filter(polygon %in% spatialExtentOfSpeciesAgeAdult)
        extremeDJuv <- extreme |>
          dplyr::filter(polygon %in% spatialExtentOfSpeciesAgeJuv)

        # Adults
        nBoxesOccupiedAdult <- length(unique(spatialExtentOfSpeciesAgeAdult))
        nBoxesOccupiedJuv <- length(unique(spatialExtentOfSpeciesAgeJuv))
        # find proportion of boxes in extremeD that relative to extent
        propBoxesAgeAdult <- length(unique(extremeDAdult$polygon)) / length(unique(spatialExtentOfSpeciesAgeAdult))
        # find proportion of time intervals in extremeD that relative to extent
        propTimeAgeAdult <- length(unique(extremeDAdult$time)) / length(unique(data$time))
        # Juveniles
        # find proportion of boxes in extremeD that relative to extent
        propBoxesAgeJuv <- length(unique(extremeDJuv$polygon)) / length(unique(spatialExtentOfSpeciesAgeJuv))
        # find proportion of time intervals in extremeD that relative to extent
        propTimeAgeJuv <- length(unique(extremeDJuv$time)) / length(unique(data$time))
      } else {
        propBoxesAgeAdult <- 0
        # find proportion of time intervals in extremeD that relative to extent
        propTimeAgeAdult <- 0
        # Juveniles
        # find proportion of boxes in extremeD that relative to extent
        propBoxesAgeJuv <- 0
        # find proportion of time intervals in extremeD that relative to extent
        propTimeAgeJuv <- 0
      }

      # All metrics need to be less than 0.01. uninhabitable areas < 1% of the total number of areas
      pass <- all(c(propBoxesRecruits,propTimeRecruits,propBoxesAgeJuv,propTimeAgeJuv,propBoxesAgeAdult,propTimeAgeAdult) < .01)

      speciesContrib <-
        data.frame(group = species,
                   layer = ilayer,
                   recruitBoxes = propBoxesRecruits,
                   ageBoxesAdult = propBoxesAgeAdult,
                   nBoxesOccupiedAdult = nBoxesOccupiedAdult,
                   ageBoxesJuv = propBoxesAgeJuv,
                   nBoxesOccupiedJuv = nBoxesOccupiedJuv,
                   recruitTime = propTimeRecruits,
                   ageTimeAdult = propTimeAgeAdult,
                   ageTimeJuv = propTimeAgeJuv,
                   pass = pass)


      outdf <- dplyr::bind_rows(outdf,speciesContrib)


    }


    # split output based on NAs. NA's indicate they are not prescribed redistribution
    # proportions. Probably because they are invertebrates

    if (is.null(outdf)) {
      tab <- NULL
    } else {
      tab <- outdf |>
        dplyr::filter(!is.na(pass),
                      pass == FALSE,
                      layer != sedimentLayer) |>
        dplyr::as_tibble()

      if (nrow(tab) == 0) {
        tab <- NULL
      }
    }


  }

  return(tab)

}
