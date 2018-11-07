prep_gage_sites_fun <- function(percentile_color_data, sites_sp, gage_style, date){
  this_date <- as.Date(date) #watch timezones if we ever switch from daily data
  #only plotting sites with color for now
  this_date_colors <- filter(percentile_color_data, dateTime == this_date)
  sites_sp@data <- left_join(sites_sp@data, this_date_colors, by = "site_no")
  plot_fun <- function(){
    #single plot of gages w/color
    plot(sites_sp, add = TRUE, pch = gage_style$pch, col = sites_sp$color, cex = gage_style$cex)
  }
  return(plot_fun)
}
