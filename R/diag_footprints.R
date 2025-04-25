#' Compares the redistribution footprints for groups in the model
#'
#'@description
#'Determine if Groups have the sam footprint for timestep redistribution
#'Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
#'vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are used to
#'compare the spatial extent of the groups.
#'
#'@param paramList A list of parameter files (Output of \code{get_atl_paramfiles()})
#'@param speciesCodes A character string of the species/group name of interest. Default is NULL (All species)
#'
#'@return A data frame. The columns are:
#'\item{group}{Species/Group name}
#'\item{adultJuv}{Logical value indicating if the adult and juvenile spatial distributions are the same}
#'\item{recruitJuv}{Logival value indicating if the juvenile and recruit spatial distributions are the same}
#'
#'
#'@family diagnostics
#'
#' @examples
#'\dontrun{
#'# Declare paths to files required
#' paramList <- list()
#' paramList$bgm.file <- "Full path to bgm file"
#' paramList$biol.prm <- "Full path to biology prm file"
#' paramList$run.prm <- "Full path to run prm file"
#' paramList$groups.file <- "Full path to group csv file"
#' # check for any species that maybe impacted by temperature
#' diag_footprints(paramList,speciesCodes=NULL)
#'
#' # check for HERRING and WHITE HAKE
#' diag_footprints(paramList,speciesCodes=c("HER","WHK"))
#'}
#'
#'@export

diag_footprints <- function(paramList, speciesCodes=NULL) {

  # check to see if all parameter files are available

  check_param_files(paramList$run.prm)
  check_param_files(paramList$bgm.file)
  check_param_files(paramList$biol.prm)
  check_param_files(paramList$groups.file)


  # get list of groups in the model
  allCodes <- atlantistools::get_acronyms(paramList$groups.file)
   if(is.null(speciesCodes)) {
    speciesCodes <- allCodes
   } else {
    # check to see if speciesCodes is in the temperatureLimits
    if (!speciesCodes %in% allCodes) {
      stop(paste0("Species Code", speciesCodes, " not found"))
    }
  }

  # get boundary boxes
  boxcoords <- atlantistools::load_box(paramList$bgm.file)
  bboxes <- atlantistools::get_boundary(boxcoords)

  # grab the horizontal redistrbution proportions for recruitment in non boundary boxes
  hdistRecruit <- get_param_recruit_hdistrib(paramList$biol.prm) |>
    dplyr::filter(!(polygon %in% bboxes),
                  value > 1e-5)
  # grab the vertical redistrbution proportions for recruitment in non boundary boxes
  vdistRecruit <- get_param_recruit_vdistrib(paramList$biol.prm) |>
    dplyr::filter(value > 1e-5)

  #get the horizontal redistribution proportions for non recruits in non boundary boxes
  hdistAge <- get_param_FXXX_SY(paramList$biol.prm)|>
    dplyr::filter(!(polygon %in% bboxes),
                  value > 1e-5)

  #get the vertical redistribution proportions for non recruits in non boundary boxes
  vdistAge <- get_param_vert(paramList$biol.prm)|>
    dplyr::filter(value > 1e-5)


  tab <- NULL
  # For each species compare spatial distribtuions
  for (species in speciesCodes) {

    # horizontal distribution of age groups
    if (species %in% unique(hdistAge$group)) {
      # for vertebrates adult same as juvenile
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
      spatialExtentOfSpeciesRecruit <- hdistRecruit |>
            dplyr::filter(group == species) |>
            dplyr::select(polygon) |>
            dplyr::distinct() |>
            dplyr::pull()

        recruitJuv <- identical(spatialExtentOfSpeciesRecruit,spatialExtentOfSpeciesAgeJuv)
        adultJuv <- identical(spatialExtentOfSpeciesAgeAdult,spatialExtentOfSpeciesAgeJuv)
    } else {
      adultJuv <- NA
      recruitJuv <- NA
    }


    speciesContrib <-
      data.frame(group = species,
                 adultJuv = adultJuv,
                 recruitJuv = recruitJuv)

    tab <- dplyr::bind_rows(tab,speciesContrib)

  }

  return(tab)

}
