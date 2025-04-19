#' Check to make sure paramater file exists
#'
#' @description
#' This function checks to make sure the parameter file exists in the specified
#' directories. If it does not, it will stop the function and return an error message.
#'
#'@param paramFile The path to the parameter file.
#'
#'@noRd
#'
#'@export

check_param_files <- function(paramFile) {
  # Check if the parameter file exists
  if (!file.exists(paramFile)) {
    stop(paste("Parameter file does not exist:", paramFile))
  }
}
