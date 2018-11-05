#' @title Get the discharge quantiles for each dv gage
#' 
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
#' @param request_limit number indicating how many sites to include per dataRetrieval request (from viz_config.yml)
#' @param percentiles character vector of the types of stats to include, i.e. `c("10", "75")` 
#' will return the 10th and 75th percentiles (from viz_config.yml)
fetch_site_stats <- function(ind_file, sites_ind, request_limit, percentiles){
   
  sites <- readRDS(scipiper::sc_retrieve(sites_ind))
  
  req_bks <- seq(1, length(sites), by=request_limit)
  stat_data <- data.frame()
  for(i in req_bks) {
    last_site <- i+request_limit-1
    get_sites <- sites[i:last_site]
    current_sites <- suppressMessages(
      dataRetrieval::readNWISstat(
        siteNumbers = get_sites,
        parameterCd = "00060", 
        statReportType="daily",
        statType=paste0("P", percentiles)
      ))
    stat_data <- rbind(stat_data, current_sites)
    print(paste("Completed", last_site, "of", length(sites)))
  }
  
  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(stat_data, data_file)
  scipiper::gd_put(ind_file, data_file)
}
