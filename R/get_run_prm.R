#' Get parameters from the run prm file
#'
#' @description
#' Pull out the parameters from the run prm file. This will only return the values of
#' parameters that have a scalar value. For example, if paramName = init_scalar, only the length
#' of the vector will be returned.
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
  lineNum <-  grep(paste0("^",paramName," "),lines)
  # extract the lines
  line <- lines[lineNum]
  linestr <- unlist(strsplit(line,"#"))[1]
  linestr <- gsub(paramName,"",linestr)
  linestr <- trimws(linestr)
  final <- unlist(strsplit(linestr,"\\s+"))
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
