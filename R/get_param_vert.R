#' Get adult distribution (vertDAY_XXX, vertNIGHT_XXX) values from a biology prm file
#'
#' @description
#' Get the values (proportion of the population) that define the distribution among layers
#' of a boxes for each 12 hour period. vertDAY_XXX and vertNIGHT_XXX are vectors. The length is the number of
#' non sediment layers. The first entry for each vector refers to water column layer
#' closest to the sediment and then up through the water column. Sediment layer is absent
#'
#'
#' @param bio.prm path to the biology prm file
#'
#' @return a data frame
#' \item{group}{Character string. Atlantis group code}
#' \item{daynight}{Character string. "day" or "night"}
#' \item{cohort}{Character string. "adult", "juvenile" or "biomass"}
#' \item{layer}{Integer. Layer number}
#' \item{value}{Value. Proportion of the population in the layer}
#'
#' @export

get_param_vert <- function(bio.prm){

  # read n parameter file
  bio.lines <-  readLines(bio.prm)
  # identify lines that contain the vertDAY, vertNIGHT values
  lineNIGHT <-  grep("^VERTnight",bio.lines)
  lineDAY <-  grep("^VERTday",bio.lines)
  # extract the lines + clean up the lines
  linesN <- bio.lines[lineNIGHT]
  linesN <- gsub("\t"," ",linesN) |>
    trimws()
  linesD <- bio.lines[lineDAY]
  linesD <- gsub("\t"," ",linesD) |>
    trimws()

  #values are on the next line. extract and clean
  valuesN <- bio.lines[lineNIGHT+1]
  valuesN <- gsub("\t"," ",valuesN)
  valuesD <- bio.lines[lineDAY+1]
  valuesD <- gsub("\t"," ",valuesD)

  # create a null dataframe
  outdf = data.frame(group = NULL, daynight = NULL,cohort = NULL,layer=NULL, value=NULL)

  # process night
  outdf <- process_vertical_distrib("night",linesN, valuesN, outdf)
  outdf <- process_vertical_distrib("day",linesD, valuesD, outdf)

  # return the data frame
  return(outdf)
}



#' Process the vertical distribution data
#'
#' @description
#' parse the day and night vectors to extract the group, cohort and values
#'
#' @return a data frame
#' \item{group}{Character string. Atlantis group code}
#' \item{daynight}{Character string. "day" or "night"}
#' \item{cohort}{Character string. "adult", "juvenile" or "biomass"}
#' \item{layer}{Integer. Layer number}
#' \item{value}{Value. Proportion of the population in the layer}
#'
#'@family internal
#'
#' @noRd

process_vertical_distrib <- function(daynightCode,linesX,valuesX, outdf) {
  # loop through the lines and extract the values, groups and the min/max
  for (i in 1:length(linesX)){
    # remove trailing part of the line where the number of values on next line is listed
    linei <- gsub("\\s+[0-9]+","",linesX[i])
    # asssign day or night
    daynight <- daynightCode
    # remove "VERTnight_" from linei
    linei <- gsub(paste0("VERT",daynightCode,"_"),"",linei)

    #check for digit in name (vertibrate) or lack of digit (invertebrate)
    if (grepl("[0-9]",linei)) { # check for digit
      # vertibrate - remove the last character in linei
      lastdigit <- substr(linei,nchar(linei),nchar(linei))
      groupi <- substr(linei,1,nchar(linei)-1)
      # use lastdigit to define juvenile or adult
      if (lastdigit == "1") {
        cohorti <- "juv"
      } else {
        cohorti <- "adult"
      }
    } else {
      groupi <- linei
      cohorti <- "biomass"
    }

    # now parse the values
    valuei <- as.numeric(unlist(strsplit(valuesX[i]," ")))
    layeri <- data.frame(layer = c(length(valuei):1),value = valuei)

    df <- data.frame(group = groupi, daynight = daynight, cohort = cohorti)

    # replicate rows of df to match the number of layers omit row names
    df <- df[rep(seq_len(nrow(df)), each = nrow(layeri)),]
    rownames(df) <- NULL

    outdf <- rbind(outdf,cbind(df,layeri))

  }
  outdf <- tibble::as_tibble(outdf)

}
