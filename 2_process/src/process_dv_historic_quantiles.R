#' @title Calculate the stat category for each gage's discharge value
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_historic_ind indicator file for the data.frame of dv_historic tmp files
#' @param percentiles character vector of the types of stats to include, i.e. `c("10", "75")`
#' will return the 10th and 75th percentiles (from viz_config.yml)
process_dv_historic_quantiles <- function(ind_file, dv_historic_ind, percentiles){

  dv_historic_data <- do.call(rbind, lapply(names(yaml.load_file(dv_historic_ind)), readRDS))

  # Find count of data so that filtering for not enough data can happen later
  dv_data_count <- dv_historic_data %>%
    group_by(site_no) %>%
    summarize(count = n())

  # Actually calculate quantiles
  dv_quantiles <- dv_historic_data %>%
    group_by(site_no) %>%
    # calculate quantiles (automatically add min and max, 0 & 1) and put into columns
    do(data.frame(t(quantile(.$Flow, probs = c(0, as.numeric(percentiles)/100, 1))))) %>%
    # rename columns to be pXX_va where XX is the percentile
    # numbers <10 automatically drop leading zero in line above and we need it back
    setNames(., gsub(pattern = "X(([0-9]){2,}).", replacement = "p\\1_va", names(.))) %>%
    setNames(., gsub(pattern = "X(([0-9]){1}).", replacement = "p0\\1_va", names(.))) %>%
    # add in data count
    left_join(dv_data_count, by = "site_no")

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_quantiles, data_file)
  scipiper::gd_put(ind_file, data_file)
}
