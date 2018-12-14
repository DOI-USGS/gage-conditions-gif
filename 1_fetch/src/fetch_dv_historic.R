#' @title Get the mean daily discharge values for every gage for their whole record.
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
#' @param dates object from viz_config.yml that specifies dates as string
#' @param request_limit number indicating how many sites to include per dataRetrieval request (from viz_config.yml)
#' @param tmp_filenames name for intermediate data files using %s syntax
fetch_dv_historic <- function(ind_file, sites_ind, dates, request_limit, tmp_filenames){

  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = '1_fetch.yml'))

  req_bks <- seq(1, length(sites), by=request_limit)
  dv_historic_data <- data.frame()
  data_file_vec <- c()
  for(i in req_bks) {
    last_site <- min(i+request_limit-1, length(sites))
    get_sites <- sites[i:last_site]

    # Check if current file already exists before pulling data
    current_filename <- sprintf(tmp_filenames, head(get_sites,1), tail(get_sites,1))
    data_file_vec <- c(data_file_vec, current_filename)
    if(!file.exists(current_filename)) {
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
      saveRDS(data_i, current_filename)
    }

    print(paste("Completed", last_site, "of", length(sites)))
  }

  # Write the indicator file as table of hashes for local files
  data_file <- scipiper::sc_indicate(ind_file = ind_file, data_file = data_file_vec)
}
