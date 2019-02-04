#' @title Fetch appropriate daily value sites from NWIS
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
fetch_flood_stage <- function(ind_file, sites_ind){

  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = '1_fetch.yml'))

  nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
  nws_flood_stage_table <- nws_flood_stage_list[["sites"]] %>%
    mutate(flood_stage = as.numeric(flood_stage)) %>%
    select(site_no, flood_stage)

  sites_with_flood_stage <- data.frame(site_no = sites, stringsAsFactors = FALSE) %>%
    left_join(nws_flood_stage_table, by = "site_no")

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(sites_with_flood_stage, data_file)
  scipiper::gd_put(ind_file, data_file)
}
