#' @title Generate a spatial points data.frame for site locations
#'
#' @param ind_file character file name where the output should be saved
#' @param site_locations_ind indicator file for the vector of site numbers
process_site_locations_to_sp <- function(site_locations_ind, projection){

  site_data <- readRDS(scipiper::sc_retrieve(site_locations_ind, remake_file = '1_fetch.yml'))
  sp_sites <- cbind(site_data$lon, site_data$lat) %>%
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>%
    sp::spTransform(CRS(projection)) %>%
    sp::SpatialPointsDataFrame(data = site_data[c('site_no', 'STATEFP')])
  return(sp_sites)
  # Write the data file and the indicator file
  # data_file <- scipiper::as_data_file(ind_file)
  # saveRDS(sp_sites, data_file)
  # scipiper::gd_put(ind_file, data_file)
}


