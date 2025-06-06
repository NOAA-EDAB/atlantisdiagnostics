% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diag_maxsize.R
\name{diag_maxsize}
\alias{diag_maxsize}
\title{Test for max fish size}
\usage{
diag_maxsize(
  nc,
  bgm,
  init,
  fgs,
  prm_run,
  prm_biol,
  speciesStats,
  speciesCodes = NULL
)
}
\arguments{
\item{nc}{A character string. Path to location of main nc file.}

\item{bgm}{A character string. Path to location of box coordinates bgm file.}

\item{init}{A character string. Path to location of initial conditions nc file.}

\item{fgs}{A character string. Path to location of functional groups file.}

\item{prm_run}{A character string. Path to location of run parameter file file.}

\item{prm_biol}{A character string. Path to location of biology parameter file.}

\item{speciesStats}{Data frame. Must contain at least 3 columns labeled \code{code} - Atlantis species codes
,\code{maxObsWeight} - a value (g) indicating the maximum weight the species should weigh,
\code{maxObsLength} - a value (cm) indicating the maximum length the species should grow to.}

\item{speciesCodes}{Character vector. A vector of Atlantis species codes in which to test for large fish.
(Default = NULL, uses all species)}
}
\value{
Returns a data frame indicating which species meet defined max weight/length criterion.

\item{species}{The common name of the species/functional group}
\item{code}{Atlantis Code for species/functional group}
\item{polygon}{Polygon in which the largest individual occupies}
\item{agecl}{Age class of the largest individual}
\item{time}{Time in which the largest individual was found (Decimal years)}
\item{maxLength}{Maximum length of species in Atlantis (cm)}
\item{maxObsLength}{Maximum observed length of species (cm) - from litereature }
\item{maxMeanWeight}{Maximum mean weight of indivdual in Atlantis (g)}
\item{maxObsWeight}{Maximum observed weight of species (g) - from litereature}
\item{passW}{Boolean indicating whether species maximum weight (from Atlantis) falls below max observed weight}
\item{ratioW}{ratio of atlantis max weight to observed max weight}
\item{passL}{Boolean indicating whether species maximum length (from Atlantis) falls below max observed length}
\item{ratioL}{ratio of atlantis max length to observed max length}
}
\description{
Calculate average size of fish through time and check to see if any fish exceed the
user supplied observed maximum size. Both maximum weight and maximum length are used
}
\seealso{
Other diagnostics: 
\code{\link{diag_cohortBiomass}()},
\code{\link{diag_fleet_catch}()},
\code{\link{diag_footprints}()},
\code{\link{diag_persistence}()},
\code{\link{diag_reasonability}()},
\code{\link{diag_temp_thresholds}()}
}
\concept{diagnostics}
