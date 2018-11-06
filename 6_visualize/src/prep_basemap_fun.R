prep_basemap_fun <- function(state_boundaries_ind, basemap_cfg) {

  state_boundaries_sp <- readRDS(sc_retrieve(state_boundaries_ind, '2_process.yml'))

  plot_fun <- function(){

    plot(state_boundaries_sp, add=TRUE,
         lwd=basemap_cfg$lwd, col=basemap_cfg$bg, border=basemap_cfg$border)

  }
  return(plot_fun)
}
