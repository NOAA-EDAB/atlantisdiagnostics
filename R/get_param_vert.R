#' Get adult distribution (vertDAY_XXX, vertNIGHT_XXX) values from a biology prm file
#'
#' @description
#' Get the values (proportion of the population) that define the distribution among layers
#' of a boxes for each 12 hour period.
#'
#' @param bio.prm path to the biology prm file
#'
#' @return a data frame with the group, cohort, value and layer
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
  out.df = data.frame(group = NULL, daynight = NULL,cohort = NULL,layer=NULL, value=NULL)
  # loop through the lines and extract the values, groups and the min/max
  for (i in 1:length(linesN)){
    # remove trailing part of the line where the number of values on next line is listed
    linei <- gsub("\\s+[0-9]+","",linesN[i])
    # check to see if string contains "night" or "day"
    if(grepl("night",linei)) {
      daynight <- "night"
      # remove "VERTnight_" from linei
      linei <- gsub("VERTnight_","",linei)
    } else {
      daynight <- "day"
      # remove "VERTnight_" from linei
      linei <- gsub("VERTday_","",linei)
    }

    #check for digit in name (vertibrate) or lack of digit (invertebrate)
    if (grepl("[0-9]",linei)) { # check for digit
      # vertibrate - remove the last character in linei
      lastdigit <- substr(linei,nchar(linei),nchar(linei))
      groupi <- substr(linei,1,nchar(linei)-1)
      # use firstdigit to define juvenile or adult
      if (groupi == "INV") {
        print(linesN[i])
        print(lastdigit)
      }
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
    valuei <- as.numeric(unlist(strsplit(valuesN[i]," ")))
    layeri <- data.frame(layer = c(length(valuei):1),value = valuei)

    df <- data.frame(group = groupi, daynight = daynight, cohort = cohorti)

    # replicate rows of df to match the number of layers omit row names
    df <- df[rep(seq_len(nrow(df)), each = nrow(layeri)),]
    rownames(df) <- NULL

    out.df <- rbind(out.df,cbind(df,layeri))

  }
  out.df <- tibble::as_tibble(out.df)

  # return the data frame
  return(out.df)
}
