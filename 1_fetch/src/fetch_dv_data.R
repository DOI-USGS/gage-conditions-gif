#' @title Download the discharge from NWIS for each dv gage
#' 
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for the vector of site numbers
#' @param dates object from viz_config.yml that specifies dates as string
fetch_dv_data <- function(ind_file, sites_ind, dates){
  
  sites <- readRDS(scipiper::sc_retrieve(sites_ind))
  
  dv_sites_data <- lapply(sites, FUN = function(x){
    d <- dataRetrieval::readNWISdata(
        service="dv",
        site = x,
        parameterCd = "00060",
        startDate = dates$start,
        endDate = dates$end) %>% 
      dataRetrieval::renameNWISColumns()
  
    if(nrow(d) > 0 && any(names(d) == "Flow")) {
      d[, c("dateTime", "Flow")] # keep only dateTime and Flow columns
    } else {
      NULL # no data returned situation
    }
    
  })
  
  names(dv_sites_data) <- sites
  
  # Write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(stat_data, data_file)
  gd_put(ind_file, data_file)
}
