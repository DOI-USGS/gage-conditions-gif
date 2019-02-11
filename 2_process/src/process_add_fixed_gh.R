#' @title Add the fixed gage height (gh) to the calculated stats data
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_data_ind indicator file for the data.frame of dv_data
#' @param fixed_gh_ind indicator file for the data.frame of dv data with fixed gh
process_add_fixed_gh <- function(ind_file, dv_stats_ind, fixed_gh_ind){

  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  fixed_gh_data <- readRDS(scipiper::sc_retrieve(fixed_gh_ind, remake_file = '1_fetch.yml'))

  # Add newly calculated gage heights to dv stats data
  dv_stats_with_fixed_gh <- dv_stats %>%
    left_join(fixed_gh_data, by = c("site_no", "dateTime")) %>%
    mutate(dv_stage = ifelse(is.na(dv_stage), GH_mean, dv_stage)) %>%
    select(-GH_mean)

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_stats_with_fixed_gh, data_file)
  scipiper::gd_put(ind_file, data_file)

}
