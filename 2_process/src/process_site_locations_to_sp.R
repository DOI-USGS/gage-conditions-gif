#' @title Generate a spatial points data.frame for site locations
#'
#' @param site_locations_ind indicator file for the vector of site numbers
#' @param projection projection string
process_site_locations_to_sp <- function(site_locations_ind, projection){

  site_data <- readRDS(scipiper::sc_retrieve(site_locations_ind, remake_file = '1_fetch.yml'))
  sp_sites <- cbind(site_data$lon, site_data$lat) %>%
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>%
    sp::spTransform(CRS(projection)) %>%
    sp::SpatialPointsDataFrame(data = site_data[c('site_no', 'STATEFP')])
  return(sp_sites)
}


