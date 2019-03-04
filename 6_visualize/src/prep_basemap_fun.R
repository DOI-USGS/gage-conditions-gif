prep_basemap_fun <- function(states_shifted, basemap_cfg) {

  # Separate islands out so they don't get cutoff by thick linewidths
  states_shifted_islands <- states_shifted[states_shifted@data$STUSPS %in% c("AK", "HI"),]
  states_shifted_conus <- states_shifted[!states_shifted@data$STUSPS %in% c("AK", "HI"),]
  rm(states_shifted)

  plot_fun <- function(){

    plot(states_shifted_conus, add=TRUE,
         lwd=basemap_cfg$lwd, col=basemap_cfg$col, border=basemap_cfg$border)
    plot(states_shifted_islands, add=TRUE,
         lwd=0.1, # 0 is not valid
         col=basemap_cfg$col, border=basemap_cfg$border)

  }
  return(plot_fun)
}
