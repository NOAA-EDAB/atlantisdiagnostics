% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diag_footprints.R
\name{diag_footprints}
\alias{diag_footprints}
\title{Compares the redistribution footprints for groups in the model}
\usage{
diag_footprints(paramList, speciesCodes = NULL)
}
\arguments{
\item{paramList}{A list of parameter files (Output of \code{get_atl_paramfiles()})}

\item{speciesCodes}{A character string of the species/group name of interest. Default is NULL (All species)}
}
\value{
A data frame. The columns are:
\item{group}{Species/Group name}
\item{adultJuv}{Logical value indicating if the adult and juvenile spatial distributions are the same}
\item{recruitJuv}{Logival value indicating if the juvenile and recruit spatial distributions are the same}
}
\description{
Determine if Groups have the sam footprint for timestep redistribution
Horizontal redistribution proportions (FXXX_SY and recruit_hdist) and
vertical distributions (vertDAY,vertNIGHT, recruit_vdistrib) are used to
compare the spatial extent of the groups.
}
\examples{
\dontrun{
# Declare paths to files required
paramList <- list()
paramList$bgm.file <- "Full path to bgm file"
paramList$biol.prm <- "Full path to biology prm file"
paramList$run.prm <- "Full path to run prm file"
paramList$groups.file <- "Full path to group csv file"
# check for any species that maybe impacted by temperature
diag_footprints(paramList,speciesCodes=NULL)

# check for HERRING and WHITE HAKE
diag_footprints(paramList,speciesCodes=c("HER","WHK"))
}

}
\seealso{
Other diagnostics: 
\code{\link{diag_cohortBiomass}()},
\code{\link{diag_fleet_catch}()},
\code{\link{diag_maxsize}()},
\code{\link{diag_persistence}()},
\code{\link{diag_reasonability}()},
\code{\link{diag_temp_thresholds}()}
}
\concept{diagnostics}
