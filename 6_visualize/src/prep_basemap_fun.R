prep_basemap_fun <- function(states_shifted, basemap_cfg) {

  plot_fun <- function(){

    plot(states_shifted, add=TRUE,
         lwd=basemap_cfg$lwd, col=basemap_cfg$col, border=basemap_cfg$border)

  }
  return(plot_fun)
}
