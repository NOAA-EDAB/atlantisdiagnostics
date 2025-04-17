#' Get parameters from the run prm file
#'
#' @description
#' Pull out the parameters from the run prm file.
#'
#'
#' @param runprm path to the run prm file
#' @param paramName name of the parameter to extract
#'
#' @return list
#' \item{value}{value of parameter}
#' \item{unit}{units of parameter, if applicable}
#'
#' @export

get_run_prm <- function(runprm, paramName = NULL ){

  if(is.null(paramName)){
    stop("Please provide a parameter name to extract")
  }

  # read n parameter file
  lines <-  readLines(runprm)
  # identify lines that contain the paramName
  lineNum <-  grep(paste0("^",paramName),lines)
  # extract the lines
  line <- lines[lineNum]
  value <- unlist(strsplit(line,"#"))[1]
  value <- gsub(paramName,"",value)
  value <- trimws(value)
  final <- unlist(strsplit(value,"\\s+"))
  out <- list()

  # grepl if any letters of characters
  if(grepl("[a-zA-Z]",final[1])){
    stop("You have misspecified the name of the parameter")
  } else {
    value <- as.numeric(final[1])
  }
  out$value <- value
  out$unit <- final[2]

  return(out)

}
