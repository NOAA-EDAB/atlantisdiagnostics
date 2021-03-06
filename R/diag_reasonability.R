#'Test for functional group reasonability
#'
#' Inspects each time point. If at any point in time a species falls below or rises above predefined
#' bounds it is flagged. The term "reasonable" is based on survey data when available.
#'
#'@param modelBiomass A data frame. Total biomass of all groups over time, read in from
#'Atlantis ...BioInd.txt output using \code{atlantisom::load_bioind}.
#'@param initialYr Numeric Scalar. Year in which the model run was initiated. (Default = 1964)
#'@param speciesCodes Character vector. A vector of Atlantis species codes in which to test for reasonableness
#'(Default = NULL, uses all species found in  \code{modelBiomass}. Species codes should be a subset of the Atlantis species codes
#'@param realBiomass A data frame. biomass time series (from assessments, stock SMART or otherwise) for species.
#' CURRENTLY IMPLEMENTED USING SURVDAT DATA ONLY  \code{realBiomass} should be in long format with column labels (YEAR,variable,value,Code,Species,isFishedSpecies)
#'  Biomass units should be in metric tonnes
#'@param nYrs Numeric scalar. Number of years from the end of the time series for which reasonableness is checked.
#' (Default = NULL, entire time series is used)
#'@param surveyBounds Numeric vector. Size of 1x2 containing the values in which to multiple lower and upper bounds of observed data.
#'For example (Default = c(1,1)) indicating use of min and max of observed biomass
#'@param initBioBounds Numeric vector. Size of 1x2 containing lower and upper bound proportions used to scale initial biomass.
#'This is used for groups/species that dont have surveys
#'@param plot A logical value specifying if the function should generate plots or
#'not. (Default = F). NOT YET IMPLEMENTED
#'
#'@importFrom magrittr %>%
#'
#'
#'@return Returns a data frame of species
#'
#'\item{species}{The common name of the species/functional group as described in Atlantis input file}
#'\item{code}{Atlantis Code for species/functional group}
#'\item{initialBiomass}{Starting value of Biomass for species/functional group. From model output}
#'\item{minBiomass}{The smallest value of biomass observed in the run}
#'\item{maxBiomass}{The largest value of biomass observed in the run}
#'\item{propInitiBio}{The maxBiomass as a proportion of the inintial biomass}
#'\item{propBelowLower}{Magnitude of lower bound excedence as a proportion of lower threshold}
#'\item{propAboveUpper}{Magnitude of upper bound excedence as a proportion of upper threshold}
#'\item{maxExceedance}{max of \code{propBelowLower} and \code{propAboveUpper}}
#'\item{t1}{The first year reasonableness was not met}
#'\item{tn}{The last year reasonableness was not met}
#'\item{nts}{The total number of years that reasonableness was not met}
#'\item{modelSkill}{Measure of the goodness of fit}
#'\item{nObs}{number of observations used in the goodness of fit}
#'\item{pass}{Boolean. Indicates if the Atlantis group passed reasonability}
#'\item{test}{Character. Indicate how \code{pass} was determined. If the Atlantis species is fished and the species is a stand alone species in the model
#'then survey data is used to assess reasonability. Otherwise deviations from initial biomass is used}
#'
#'
#'@family diags
#'
#'@export
#'
#'
#'@examples
#'\dontrun{
#' # Declare paths to files required
#' biol.file <- "neus_outputBiomIndx.txt"
#' file_fgs <- "neus_groups.csv"
#' # use atlantisom to read them in
#' fgs <- atlantisom::load_fgs(inDir,file_fgs)
#' modelBiomass <- atlantisom::load_bioind(outDir,biol.file,fgs)
#' # read in survey biomass and convert to metric tons
#' realBiomass <- readRDS(paste0(dataDir,"sweptAreaBiomassNEUS.rds")) %>%
#'       dplyr::filter(variable %in% c("tot.biomass")) %>%
#'       dplyr::mutate(value=ifelse(grepl("kg$",units),value/1000,value)) %>%
#'       dplyr::select(-units)
#'
#' # Perform reasonability test on all species/groups using the last 20 years of the run.
#' # Allow species with data to be bounded by 100 x max observed biomass and 1 x min observed biomass.
#' # For species without data allow model biomass to lie between 0.5 and 2 times initial biomass
#'
#' diag_reasonability(modelBiomass=modelBiomass, initialYr = 1964, realBiomass=realBiomass,
#' surveyBounds = c(1,100), initBioBounds = c(0.5,2))
#'
#' # Only perform test on herring and white hake.
#' diag_reasonability(modelBiomass=modelBiomass, initialYr = 1964, speciesCodes =c("MAK","WHK"), realBiomass=realBiomass,
#' surveyBounds = c(1,100), initBioBounds = c(0.5,2))
#'}

diag_reasonability <- function(modelBiomass, initialYr=1964, speciesCodes=NULL, realBiomass, nYrs = NULL,
                               surveyBounds = c(1,1), initBioBounds = c(0.5,10), plot=F){

  ################################################
  ########### model output #######################
  ################################################

  # check for valid species Codes & clean
  speciesCodes <- check_species_codes(modelBiomass,speciesCodes)

  # list of Atlantis species and Atlantis codes. Pulled from model output
  species <- modelBiomass %>%
    dplyr::select(species,code) %>%
    dplyr::filter(!is.na(code)) %>%
    dplyr::distinct(species,code)

  # pull initial biomass for each species/group
  initialBiomass <- modelBiomass %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(initialBiomass = dplyr::first(atoutput)) %>%
    dplyr::select(species,code,initialBiomass) %>%
    dplyr::distinct(species,code,initialBiomass)

  # filter model biomass and average over the year
  # join with initial biomass
  modelBiomass <- modelBiomass %>%
    dplyr::select(time, atoutput, code, species) %>%
    dplyr::rename(value=atoutput) %>%
    dplyr::mutate(yearStep= floor(time/365)) %>%
    dplyr::group_by(code,yearStep,species) %>%
    dplyr::summarise(modelBiomass = mean(value),.groups="drop") %>%
    dplyr::mutate(year = yearStep+initialYr,yearStep=NULL) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(.,initialBiomass,by=c("code","species")) %>%
    tibble::as_tibble()

  ################################################
  ############## observed data ###################
  ################################################

  # filter real biomass data to select only species that are fished in the atlantis model
  # then remove sharks, whales, birds etc if present.
  speciesBiomass <- realBiomass %>%
    dplyr::filter(isFishedSpecies == T) %>%
    dplyr::filter(!grepl("shark",Species)) %>%
    dplyr::filter(!grepl("whale",Species)) %>%
    dplyr::filter(!grepl("bird",Species)) %>%
    dplyr::filter(!grepl("salmon",Species)) %>%
    dplyr::select(YEAR,variable,value,Code,Species) %>%
    dplyr::rename(year=YEAR,code=Code)

  ## some codes have multiple species associated
  # aggregate biomass over all species within a code
  # metric tonnes
  speciesBiomass <- speciesBiomass %>%
    dplyr::group_by(year,variable,code) %>%
    dplyr::summarise(realBiomass = sum(value),nSpecies = dplyr::n(),.groups="drop") %>%
    tidyr::pivot_wider(.,names_from = variable,values_from = realBiomass)

  # find final year of model run
  maxRuntime <- max(modelBiomass$year)

  # determine time frame in which to perform "test"
  if (is.null(nYrs)) { # use all time series
    filterTime <- initialYr
  } else { # last n years
    filterTime <- maxRuntime - nYrs
  }

  # now find if model biomass falls within real biomass * bounds
  # need to map model time to real time.
  # use %age of initial biomass if no real Biomass
  reasonable <- NULL
  # loop over species
  for (acode in speciesCodes) {

    # filter real/observed data by species and the time frame
    rb <- speciesBiomass %>%
      dplyr::filter(code == acode) %>%
      dplyr::filter(year > filterTime)


    # if data available for species code then get model data for time range
    if (nrow(rb)==0) { # no data
      # take %age of initialbiomass

      # compare model biomass output against scaled initial biomass
      # calculate goodness of fit (mef)
      mb <- modelBiomass %>%
        dplyr::filter(code == acode) %>%
        dplyr::filter(year > filterTime) %>%
        dplyr::mutate(reasonable = (modelBiomass < initialBiomass*initBioBounds[2]) & (modelBiomass >= initialBiomass*initBioBounds[1])) %>%
        dplyr::mutate(modelSkill = calc_mef(modelBiomass,initialBiomass)$mef) %>%
        dplyr::mutate(minBiomass = min(modelBiomass)) %>%
        dplyr::mutate(maxBiomass = max(modelBiomass)) %>%
        dplyr::mutate(propInitBio = maxBiomass/initialBiomass) %>%
        dplyr::mutate(propAboveUpper = maxBiomass/(initialBiomass*initBioBounds[2]) - 1) %>%
        dplyr::mutate(propBelowLower = -minBiomass/(initialBiomass*initBioBounds[1]) + 1) %>%
        dplyr::group_by(code) %>%
        dplyr::mutate(maxExceedance = max(propBelowLower,propAboveUpper)) %>%
        dplyr::ungroup()

      test <- "initialBio"

    } else { # if complete data present

      # compare model biomass against scaled survey estimates
      # calculate goodness of fit (mef)
      mb <- modelBiomass %>%
        dplyr::filter(code == acode) %>%
        dplyr::filter(year > filterTime) %>%
        dplyr::left_join(.,rb,by=c("code"="code","year"="year")) %>%
        dplyr::mutate(reasonable = (modelBiomass < max(tot.biomass)*surveyBounds[2]) & (modelBiomass > min(tot.biomass)*surveyBounds[1])) %>%
        dplyr::mutate(modelSkill = calc_mef(tot.biomass,modelBiomass)$mef) %>%
        dplyr::mutate(minBiomass = min(modelBiomass)) %>%
        dplyr::mutate(maxBiomass = max(modelBiomass)) %>%
        dplyr::mutate(propInitBio = maxBiomass/initialBiomass) %>%
        dplyr::mutate(propAboveUpper = maxBiomass/(max(tot.biomass)*surveyBounds[2]) - 1) %>%
        dplyr::mutate(propBelowLower = -minBiomass/(min(tot.biomass)*surveyBounds[1]) + 1) %>%
        dplyr::group_by(code) %>%
        dplyr::mutate(maxExceedance = max(propBelowLower,propAboveUpper)) %>%
        dplyr::ungroup()

      test <- "data"
    }

    # filter all years failing test
    out <- mb %>%
      dplyr::filter(reasonable == F)


    if(nrow(out) == 0) { # all pass
      # create output
      out <- mb %>%
        dplyr::select(code,species,initialBiomass,modelSkill,minBiomass,maxBiomass,propInitBio,propBelowLower,propAboveUpper,maxExceedance) %>%
        dplyr::distinct() %>%
        dplyr::mutate(t1 = NA) %>%
        dplyr::mutate(tn = NA) %>%
        dplyr::mutate(nts = 0) %>%
        dplyr::mutate(pass = T) %>%
        dplyr::mutate(test = test)
    } else { # some failure
      out <- mb %>%
        dplyr::filter(reasonable == F) %>%
        dplyr::mutate(t1 = min(year)) %>%
        dplyr::mutate(tn = max(year)) %>%
        dplyr::group_by(code,species,initialBiomass,t1,tn,modelSkill,minBiomass,maxBiomass,propInitBio,propBelowLower,propAboveUpper,maxExceedance) %>%
        dplyr::summarize(nts = sum(!reasonable),.groups="drop") %>%
        dplyr::mutate(pass = dplyr::if_else(nts>0,F,T)) %>%
        dplyr::mutate(test = test)

    }

    # bind to main output data frame
    reasonable <- rbind(reasonable,out)

  }
  reasonable <- reasonable %>%
    dplyr::arrange(pass,desc(maxExceedance))

  return(reasonable)


}


