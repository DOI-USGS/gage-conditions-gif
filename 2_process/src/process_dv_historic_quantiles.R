#' @title Calculate the stat category for each gage's discharge value
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_historic_ind file with the data.frame of flow data filtered to just
#' current sites from the national-flow-observations pipeline
#' @param percentiles character vector of the types of stats to include, i.e. `c("10", "75")`
#' will return the 10th and 75th percentiles (from viz_config.yml)
process_dv_historic_quantiles <- function(ind_file, dv_historic_ind, percentiles){

  dv_quantiles <- readRDS(sc_retrieve(dv_historic_ind, remake_file = '2_process.yml')) %>%
    split(.$site_no) %>%
    purrr::map(function(df) {
      # calculate quantiles (automatically add min and max, 0 & 1) and put into columns
      t(quantile(df$flow_cfs, probs = c(0, as.numeric(percentiles)/100, 1), na.rm = T)) %>%
        data.frame() %>%
        mutate(site_no = unique(df$site_no),
               count = length(df$flow_cfs)) %>%
        select(site_no, everything())
    }) %>%
    purrr::reduce(bind_rows) %>%
    setNames(., gsub(pattern = "X(([0-9]){2,}).", replacement = "p\\1_va", names(.))) %>%
    setNames(., gsub(pattern = "X(([0-9]){1}).", replacement = "p0\\1_va", names(.)))

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_quantiles, data_file)
  scipiper::gd_put(ind_file, data_file)
}

filter_historic_to_current_sites <- function(ind_file, dv_historic_fn, site_ind) {

  data_file <- scipiper::as_data_file(ind_file)
  sites <- readRDS(scipiper::sc_retrieve(site_ind, remake_file = '1_fetch.yml'))

  readRDS(dv_historic_fn) %>%
    # rename since it is called "site_id" in national-flow-observations
    rename(site_no = site_id) %>%
    filter(site_no %in% sites) %>%
    saveRDS(data_file)

  scipiper::gd_put(ind_file, data_file)

}
