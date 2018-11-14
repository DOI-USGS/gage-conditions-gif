#' @title Calculate the stat category for each gage's discharge value
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_data_ind indicator file for the data.frame of dv_data
#' @param site_stats_clean_ind indicator file for the data.frame of dv stats for each site
#' @param dates object from viz_config.yml that specifies dates as string
#' @param percentiles character vector of the types of stats to include, i.e. `c("10", "75")`
#' will return the 10th and 75th percentiles (from viz_config.yml)
process_dv_stats <- function(ind_file, dv_data_ind, site_stats_clean_ind, dates, percentiles){

  dv_data <- readRDS(scipiper::sc_retrieve(dv_data_ind, remake_file = '1_fetch.yml'))
  site_stats <- readRDS(scipiper::sc_retrieve(site_stats_clean_ind, remake_file = '2_process.yml'))
  # breakdown date into month & day pairs
  dv_data_md <- dv_data %>%
    dplyr::mutate(month_nu = as.numeric(format(dateTime, "%m")),
                  day_nu = as.numeric(format(dateTime, "%d")))
  #calculate median of medians for each site
  assert_that(sum(is.na(site_stats$p50_va)) == 0)

  #once https://github.com/USGS-VIZLAB/gage-conditions-gif/issues/48 is dealt with this
  #assertion should be true
  #assertthat((site_stats %>% group_by(site_no) %>% summarize(n=n()) %>% pull(n) %>% max()) == 366)
  sites_overall_medians <- site_stats %>% group_by(site_no) %>% summarize(site_median = median(p50_va))

  # merge stats with the dv data
  # merge still results in extra rows - 24 extra to be exact
  dv_with_stats <- left_join(dv_data_md, site_stats, by = c("site_no", "month_nu", "day_nu")) %>%
    left_join(sites_overall_medians, by = "site_no")

  stat_colnames <- sprintf("p%s_va", percentiles)
  stat_perc <- as.numeric(percentiles)/100

  interpolate_percentile <- function(df){
    # This function takes the current daily value and interpolates it's percentile based
    # on the percentiles for the matching site and day of the year
    df <- select(df, "dv_val", one_of(stat_colnames))
    out <- rep(NA, nrow(df))

    for (i in 1:length(out)){
      dv_val <- df$dv_val[i]

      df_i <- slice(df, i) %>%
        select(-dv_val) %>%
        tidyr::gather(stat_name, stat_value) %>%
        mutate(stat_value = as.numeric(stat_value),
               stat_type = as.numeric(gsub("p|_va", "", stat_name))/100)

      y <- df_i$stat_type
      x <- df_i$stat_value
      nas <- is.na(x)
      x <- x[!nas]
      y <- y[!nas]
      if (length(unique(x)) < 2){
        out[i] <- NA
      } else if (dv_val < x[1L]){ # the first and last *have* to be numbers per filtering criteria
        out[i] <- head(stat_perc, 1)
      } else if (dv_val > tail(x, 1L)){
        out[i] <- tail(stat_perc, 1)
      } else {
        out[i] <- approx(x, y, xout = dv_val)$y
      }
    }
    return(out)

  }
  dv_stats <- dv_with_stats %>%
    mutate(dv_val = Flow) %>%
    filter_(sprintf("!is.na(%s)", stat_colnames[1]),
            sprintf("!is.na(%s)", tail(stat_colnames,1)),
            sprintf("!is.na(%s)", "dv_val")) %>%
    mutate(per = interpolate_percentile(.)) %>%
    select(site_no, dateTime, dv_val, per, p50_va, site_median)
  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_stats, data_file)
  scipiper::gd_put(ind_file, data_file)
}
