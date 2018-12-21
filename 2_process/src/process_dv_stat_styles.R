#' @title Compute the style of each point: color for each daily value percentile (reflecting anomalizes relative to day of year) and size for the typical flow on this day of year relative to flow on other days of year at the same site (reflecting typical seasonlity of flow).
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param gage_style list indicating point type and size to use for gages with or without percentiles
#' @param display_percentiles list with percentiles for normal_range, drought_severe, drought_low, and flood
process_dv_stat_styles <- function(ind_file, dv_stats_ind, gage_style, display_percentiles){

  # read in the prepared statistics
  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  display_percentiles_num <- lapply(display_percentiles, function(x) as.numeric(x)/100)

  # widen the range for "normal"
  norm_per_low <- min(display_percentiles_num$normal_range)
  norm_per_high <- max(display_percentiles_num$normal_range)
  dv_stats_adj <- dv_stats %>%
    mutate(per_adj = ifelse(per >= norm_per_low & per <= norm_per_high,
                            no = per, yes = 0.5))

  # set the styles
  dv_stats_with_style <- dv_stats_adj %>%
    add_style_columns(gage_style, display_percentiles_num)

  # insert values into the color column. this was not working with ifelse inside
  # mutate, still NAs getting into rgb/col_fun
  per_na <- is.na(dv_stats_with_style$per_adj)
  dv_stats_with_style$color[per_na] <- gage_style$no_percentile$col
  dv_stats_with_style$size[per_na] <- gage_style$no_percentile$cex
  dv_stats_with_style$shape[per_na] <- gage_style$no_percentile$pch

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_style, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}

add_style_columns <- function(per_df, gage_style, percentiles) {
  mutate(per_df,
    color = ifelse(per <= min(percentiles$normal_range),
                   yes = gage_style$with_percentile$drought$col,
                   no = ifelse(per >= percentiles$flood,
                               yes = gage_style$with_percentile$flood$col[2],
                               no = ifelse(per >= max(percentiles$normal_range),
                                           yes = gage_style$with_percentile$flood$col[1],
                                           no = gage_style$with_percentile$normal$col))),
    shape = ifelse(per <= min(percentiles$normal_range),
                   yes = gage_style$with_percentile$drought$pch,
                   no = gage_style$with_percentile$normal$pch),
    size = ifelse(per <= percentiles$drought_severe,
                  yes = max(gage_style$with_percentile$drought$cex),
                  no = ifelse(per <= percentiles$drought_low,
                              yes = min(gage_style$with_percentile$drought$cex),
                              no = gage_style$with_percentile$normal$cex))
  )
}
