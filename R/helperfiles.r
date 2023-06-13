#' Helper files
#'
#' Internal to package
#' Generate paths of param files from run command
#'
#' @param run.cmd Content of run (bat) file
#' @param code flag that denotes parameter file
#'
#' @noRd


run.filename <- function(run.cmd,code){
  return(strsplit(strsplit(run.cmd,paste0(code,' '))[[1]][2],' ')[[1]][1])
}


#Utility function
bind.save = function(x,name,out.dir){
  x2 = dplyr::bind_rows(x)
  saveRDS(x2,file.path(out.dir,paste0(name,'.rds')))
}


#Utility function
add.title = function(p,title){
  p = p + ggplot2::ggtitle(title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  return(p)
}

# plotting
plot_sp <- function(data, col, wrap_col) {
  if (nrow(data) == 0) {
    plot <- ggplot2::ggplot() + ggplot2::theme_void()
  }
  else {
    agg_data <- agg_data(data, groups = col, out = "sum_diet",
                         fun = sum)
    data[, col] <- factor(data[[col]], levels = agg_data[[1]][order(agg_data$sum_diet,
                                                                    decreasing = TRUE)])
    plot <- ggplot2::ggplot(data, ggplot2::aes_(x = ~time,
                                                y = ~atoutput, fill = lazyeval::interp(~var,
                                                                                       var = as.name(col)))) + ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = c(get_colpal(),RColorBrewer::brewer.pal(8,'Set1'))) +
      ggplot2::facet_wrap(lazyeval::interp(~var, var = as.name(wrap_col)),
                          ncol = 5, labeller = "label_both") + ggplot2::labs(x = NULL,
                                                                             y = NULL, title = NULL) + theme_atlantis() +
      ggplot2::theme(legend.position = "right")
    plot <- atlantistools:::ggplot_custom(plot)
  }
  return(plot)
}
