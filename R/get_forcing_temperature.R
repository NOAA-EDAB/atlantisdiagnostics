#' Read in temperature forcing by box or from output.nc
#'
#'@description
#' To ge a better idea of how temperature changes in space
#' and time
#'
#' Important to discern if species distribution changes are due
#' to changes in temperature or something else or to identify which regions
#' a species might leave if temperature was increased
#' under climate scenarios
#'
#' @param param.ls list. List of file paths from Atlantis model
#' @param plotFigs logical. If TRUE, plots are generated
#'
#' @return tibble. Data frame of temperature data by box/layer/time


get_forcing_temperature <- function(param.ls, plotFigs=F){

  # open output.nc file
  # get temperature data on time step of output
  # outputInterval = 73
  # Value per box/layer /time (time = every 73 days)
  # dim = 5 x 30 x 286 (layer x box x time)
  # Note: (286-1) x 73 /365 = 57 The number of years the model is run for 1964-2020
  # First output is time 0. Ignore this
  # note: disregard boxes 1, 24-30 since they are boundary boxes. These are the slots in the nc file.
  # these boundary boxes are actually labelled (0,23-29 boundary boxes).
  # So box 0 is index 1 in nc file

  # read in temperature data from output nc file
  boxcoords <- atlantistools::load_box(param.ls$bgm.file)
  bboxes <- atlantistools::get_boundary(boxcoords)
  tempData <- atlantistools::load_nc_physics(nc = param.ls$main.nc,
                                 select_physics = "Temp",
                                 prm_run = param.ls$run.prm,
                                 bboxes = bboxes)

  # relabel the layers to make more sense across polygons and time
  # the order of the layers are reversed in the output.nc file
  # 3,2,1,0,4 (4 = sediment) but are labelled 0,1,2,3,4 (4 =sediment)

  tempD <- tempData |>
    dplyr::group_by(polygon,time) |>
    dplyr::mutate(layer = dplyr::n()-2-layer) |>
    dplyr::mutate(layer = dplyr::case_when(layer < 0 ~ 4,
                                            .default = layer)) |>
    dplyr::ungroup() |>
    dplyr::mutate(layer = as.factor(layer))

  ## Histogram of temperatures by box
  # ggplot2::ggplot(data = tempD) +
  #   ggplot2::geom_boxplot(ggplot2::aes(layer,atoutput,group=layer)) +
  #   ggplot2::facet_wrap(~polygon) +
  #   ggplot2::ylab("Temperature (\u00b0C)")
  #
  # ## Histogram of temperatures by box
  # ggplot2::ggplot(data = tempD) +
  #   ggplot2::geom_boxplot(ggplot2::aes(atoutput,layer,group=layer)) +
  #   ggplot2::facet_wrap(~polygon) +
  #   ggplot2::xlab("Temperature (\u00b0C)")

  ###################################################
  ### boxplots of temperature by layer and polygon
  ###################################################

  tempD2 <- tempD
  tempD2$layer <- factor(tempD2$layer, levels = rev(levels(tempD2$layer)))
  p <- ggplot2::ggplot(data = tempD2) +
    ggplot2::geom_boxplot(ggplot2::aes(atoutput,layer,group=layer),outlier.size = 0.5) +
    ggplot2::facet_wrap(~polygon) +
    ggplot2::xlab("Temperature (\u00b0C)") +
    ggplot2::ylab("Layer (0 = Surface, 4 = Sediment)") +
    ggplot2::ggtitle("Temperature distribution by polygon/layer")

  if(plotFigs) {
    print(p)
  }

  ###################################################
    # timeseriers plots by polygon/layer
  ###################################################
  if(plotFigs) {

    grid <- expand.grid(polygon = unique(tempD$polygon),
                        time = unique(tempD$time))
    grid <- grid |>
      dplyr::as_tibble()
    # round up to nearest decade for plotting
    maxTime <- max(tempD$time) + (10 - max(tempD$time) %% 10)
    for (ilayer in sort(unique(tempD$layer))) {
      data = tempD |>
        dplyr::filter(layer == ilayer) |>
        dplyr::select(-variable,-layer)
      if (nrow(data) == 0) {
        next
      }
      dataN <- grid |>
        dplyr::left_join(data,by= c("polygon","time")) |>
        dplyr::as_tibble()

      p <- ggplot2::ggplot(dataN) +
        ggplot2::geom_line(ggplot2::aes(x = as.numeric(time),y=atoutput),na.rm = T) +
        ggplot2::facet_wrap(~polygon) +
        ggplot2::ylab("Temperature (\u00b0C)") +
        ggplot2::xlab("Model Time (t = 73 days)") +
        ggplot2::ggtitle(paste0("Temperature by polygon for layer ",ilayer)) +
        ggplot2::ylim(c(min(tempD$atoutput),max(tempD$atoutput))) +
        ggplot2::xlim(c(min(tempD$time),maxTime))

      print(p)
    }
  }

  return(tempD2)


}
