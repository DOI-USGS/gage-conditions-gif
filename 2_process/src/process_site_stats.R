#' @title Clean up the site statistics data to eliminate duplicates
#' 
#' @param ind_file character file name where the output should be saved
#' @param site_stats_ind indicator file for the data frame of site statistics
process_site_stats <- function(ind_file, site_stats_ind){
  
  stat_data <- readRDS(scipiper::sc_retrieve(site_stats_ind, remake_file = '1_fetch.yml'))
  
  # For duplicated site stats, pick the result with the more recent end_yr
  #   E.g. Site number 12010000 has two sets of stats for some of it's data 
  #   filter by January 1 and you will see one set from 1930 - 2003 and one 
  #   from 1930 - 2018. Filter so that only the 2018 one is used.
  stat_data_unique <- stat_data %>%
    dplyr::mutate(nyears = end_yr - begin_yr) %>% 
    tidyr::unite(mashed, site_no, month_nu, day_nu) %>% 
    dplyr::distinct() %>% # some of the stats are literally exact copies
    dplyr::group_by(mashed) %>% 
    dplyr::mutate(same_window = any(duplicated(nyears))) %>% 
    dplyr::filter(ifelse(!same_window, 
                         # pick the stat that has more years of data
                         nyears == max(nyears), 
                         # if there are > 1 with the same number of years,
                         # pick the more recent stat
                         end_yr == max(end_yr))) %>% 
    dplyr::ungroup() %>% 
    tidyr::separate(mashed, c("site_no", "month_nu", "day_nu"), sep = "_") %>% 
    dplyr::mutate(month_nu = as.numeric(month_nu), day_nu = as.numeric(day_nu))

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(stat_data_unique, data_file)
  scipiper::gd_put(ind_file, data_file)
}
