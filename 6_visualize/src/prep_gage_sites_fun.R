prep_gage_sites_fun <- function(percentile_color_data_ind, sites_sp, gage_style, dateTime){
  this_date <- as.POSIXct(dateTime, tz = "UTC") #watch timezones if we ever switch from daily data
  #only plotting sites with color for now
  percentile_color_data <- readRDS(as_data_file(percentile_color_data_ind))
  this_date_colors <- filter(percentile_color_data, dateTime == this_date)
  sites_sp@data <- left_join(sites_sp@data, this_date_colors, by = "site_no")

  gage_sites_plot_fun <- function(){
    #single plot of gages w/color
    plot(sites_sp, add = TRUE, pch = gage_style$pch, col = sites_sp$color, cex = gage_style$cex)
  }
  return(gage_sites_plot_fun)
}
