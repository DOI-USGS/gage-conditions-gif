#' @title Generate a spatial points data.frame for site locations
#' 
#' @param ind_file character file name where the output should be saved
#' @param site_locations_ind indicator file for the vector of site numbers
process_site_locations_to_sp <- function(ind_file, site_locations_ind){
  
  site_data <- readRDS(scipiper::sc_retrieve(site_locations_ind, remake_file = '1_fetch.yml'))
  
  coords <- cbind(site_data$lon, site_data$lat)
  sp_sites <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(site_data, lat, lon), 
    data = dplyr::select(site_data, -lat, -lon), 
    proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
  
  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(sp_sites, data_file)
  scipiper::gd_put(ind_file, data_file)
}
