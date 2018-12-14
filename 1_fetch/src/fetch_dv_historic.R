#' @title Get the mean daily discharge values for every gage for their whole record.
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
#' @param dates object from viz_config.yml that specifies dates as string
#' @param request_limit number indicating how many sites to include per dataRetrieval request (from viz_config.yml)
fetch_dv_historic <- function(ind_file, sites_ind, dates, request_limit){

  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = '1_fetch.yml'))

  req_bks <- seq(1, length(sites), by=request_limit)
  dv_historic_data <- data.frame()
  for(i in req_bks) {
    last_site <- min(i+request_limit-1, length(sites))
    get_sites <- sites[i:last_site]
    data_i <-
      dataRetrieval::readNWISdata(
        service = "dv",
        statCd = "00003", # need this to avoid NAs
        site = get_sites,
        parameterCd = "00060",
        startDate = "1800-01-01",
        endDate = dates$start) %>%
      dataRetrieval::renameNWISColumns()

    if(nrow(data_i) > 0 && any(names(data_i) == "Flow")) {
      data_i <- data_i[, c("site_no", "dateTime", "Flow")] # keep only dateTime and Flow columns
    } else {
      data_i <- NULL # no data returned situation
    }

    # save individual file locally as building in case something happens
    saveRDS(data_i, paste0("1_fetch/tmp/historic_dv_", head(get_sites,1), "_to_", tail(get_sites,1), ".rds"))

    dv_historic_data <- rbind(dv_historic_data, data_i)
    print(paste("Completed", last_site, "of", length(sites)))
  }

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_historic_data, data_file)
  scipiper::gd_put(ind_file, data_file)
}
