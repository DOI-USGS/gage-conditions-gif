#' @title Calculate the stat category for each gage's discharge value
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_data_ind indicator file for the data.frame of dv_data
#' @param site_stats_ind indicator file for the data.frame of dv stats for each site
#' @param dates object from viz_config.yml that specifies dates as string
process_dv_stats <- function(ind_file, dv_data_ind, site_stats_ind, dates){

  dv_data <- readRDS(scipiper::sc_retrieve(dv_data_ind, remake_file = '1_fetch.yml'))
  site_stats <- readRDS(scipiper::sc_retrieve(site_stats_ind, remake_file = '2_process.yml'))

  dv_with_stats <- left_join(dv_data, site_stats, by = "site_no") %>%
    mutate(dv_val = Flow)

  stat_colnames <- names(site_stats)[grepl("p[0-9]+_va", names(site_stats))]
  stat_perc <- as.numeric(gsub("p([0-9]+)_va", "\\1", stat_colnames))/100

  interpolate_percentile <- function(df){
    # This function takes the current daily value and interpolates its percentile based
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
      if (is.na(dv_val)) {
        out[i] <- NA
      } else if (length(unique(x)) < 2){
        out[i] <- NA
      } else if (dv_val < x[1L]){
        # less than the minimum, makes it the new minimum
        out[i] <- 0
      } else if (dv_val > tail(x, 1L)){
        # greater than maximum, makes it the new maximum
        out[i] <- 1
      } else {
        out[i] <- approx(x, y, xout = dv_val)$y
      }
      print(i)
    }
    return(out)

  }

  # Add NA for short record (need at least 3 years of data or when dv_val is NA
  dv_data_nas <- dv_with_stats %>%
    filter(is.na(dv_val) | count < 365*3) %>%
    mutate(per = NA) %>%
    select(site_no, dateTime, dv_val, per, p50_va)

  # Long enough record and no missing dv_val, so perform interpolation
  dv_data <- dv_with_stats %>%
    filter(!is.na(dv_val) & count >= 365*3) %>%
    mutate(per = interpolate_percentile(.)) %>%
    select(site_no, dateTime, dv_val, per, p50_va) %>%
    bind_rows(dv_data_nas)

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_stats, data_file)
  scipiper::gd_put(ind_file, data_file)
}
