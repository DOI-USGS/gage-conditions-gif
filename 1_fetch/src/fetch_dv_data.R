#' @title Download the discharge from NWIS for each dv gage
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
#' @param dates object from viz_config.yml that specifies dates as string
#' @param request_limit number indicating how many sites to include per dataRetrieval request (from viz_config.yml)
fetch_dv_data <- function(ind_file, sites_ind, dates, request_limit){

  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = '1_fetch.yml'))

  req_bks <- seq(1, length(sites), by=request_limit)
  dv_data <- data.frame()

  for(i in req_bks) {
    last_site <- min(i+request_limit-1, length(sites))
    get_sites <- sites[i:last_site]
    data_i <-
      dataRetrieval::readNWISdata(
        service = "dv",
        statCd = "00003", # need this to avoid NAs
        site = get_sites,
        parameterCd = c("00065", "00060"),
        startDate = dates$start,
        endDate = dates$end) %>%
      dataRetrieval::renameNWISColumns()

    if(nrow(data_i) > 0 && any(names(data_i) == "Flow")) {

      if(!"GH" %in% names(data_i)) {
        # Not all come back with a GH column, but need it to combine with everything
        data_i$GH <- NA
      }

      data_i <- data_i[, c("site_no", "dateTime", "Flow", "GH")] # keep only dateTime, Flow, and gage height columns
    } else {
      data_i <- NULL # no data returned situation
    }

    dv_data <- rbind(dv_data, data_i)
    message(paste("Completed", last_site, "of", length(sites)))
  }

  dv_data_unique <- dplyr::distinct(dv_data) # need this to avoid some duplicates

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_data_unique, data_file)
  scipiper::gd_put(ind_file, data_file)
}
