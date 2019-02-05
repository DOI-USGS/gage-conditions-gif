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

  # Learned that these sites near Jacksonville, FL were flooding every day.
  # So, we are ignoring that they have a flood stage.
  # 02244440: flood_stage = 2, but mininum stage for WY18 = 10.54
  # 02246459: flood_stage = 2, but mininum stage for WY18 = 10.68
  # 02246500: flood_stage = 2, but mininum stage for WY18 = 10.17

  sites_with_flood_stage <- data.frame(site_no = sites, stringsAsFactors = FALSE) %>%
    left_join(nws_flood_stage_table, by = "site_no") %>%
    filter(!site_no %in% c("02244440", "02246459", "02246500"))

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(sites_with_flood_stage, data_file)
  scipiper::gd_put(ind_file, data_file)
}
