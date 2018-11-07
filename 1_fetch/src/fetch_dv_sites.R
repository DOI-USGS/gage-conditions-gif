#' @title Fetch appropriate daily value sites from NWIS
#'
#' @param ind_file character file name where the output should be saved
#' @param dates object from viz_config.yml that specifies dates as string
fetch_dv_sites <- function(ind_file, dates){

  hucs <- zeroPad(1:21, 2) # all hucs

  sites <- c()
  for(huc in hucs){
    sites <-
      dataRetrieval::whatNWISdata(
        huc = huc,
        service = "dv",
        startDate = dates$start,
        endDate = dates$end,
        parameterCd = "00060",
        statCd = "00003") %>%
      dplyr::pull(site_no) %>%
      unique() %>%
      c(sites)
  }

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(sites, data_file)
  scipiper::gd_put(ind_file, data_file)
}
