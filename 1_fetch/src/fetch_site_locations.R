#' @title Fetch the latitude and longitude for sites
#' 
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
fetch_site_locations <- function(ind_file, sites_ind){
  
  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = '1_fetch.yml'))
  
  site_data <- 
    dataRetrieval::readNWISsite(sites) %>% 
    dplyr::select(
      site_no, 
      STATEFP = state_cd, 
      lon = dec_long_va, 
      lat = dec_lat_va) %>% 
    dplyr::filter(!is.na(lon), !is.na(lat))

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(site_data, data_file)
  scipiper::gd_put(ind_file, data_file)
}
