#' @title Generate a projected spatial points data.frame for site locations
#'
#' @param site_locations_ind indicator file for the data_frame of site coordinates
#' @param projection projection string
project_points <- function(site_locations_ind, projection) {

  site_data <- readRDS(scipiper::sc_retrieve(site_locations_ind, remake_file = '1_fetch.yml'))
  sites_mutated_sp <- cbind(site_data$lon, site_data$lat) %>%
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>%
    sp::spTransform(CRS(projection)) %>%
    sp::SpatialPointsDataFrame(data = site_data[c('site_no', 'STATEFP')])
}

#' @title Shift the projected spatial site locations with reference to the states
#'
#' @param projected_points the projected site points sp object
#' @param projected_states the projected states sp object
#' @param shift_cfg list of scale/shift/rotate configuration parameters
shift_points <- function(projected_points, projected_states, shift_cfg) {

  mutate_sp_coords(
    projected_points, ref = projected_states,
    STATEFP = shift_cfg$STATEFP, scale = shift_cfg$scale,
    shift_x = shift_cfg$shift_x, shift_y = shift_cfg$shift_y, rotate = shift_cfg$rotate)
}
