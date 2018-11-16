#' @title Compute the color for each daily value percentile
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
process_seasonality_colors <- function(ind_file, dv_stats_ind, color_palette, gage_style){

  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  col_fun <- colorRamp(color_palette$with_percentile)

  dv_stats_scaled <- dv_stats %>%
    # calculate difference from site_median
    mutate(delta = dv_val - site_median,
           per_diff = ifelse(delta == 0, yes = 0,
                             # if the change is zero, then the percent difference is 0
                             no = ifelse(site_median == 0,
                                         # if the site_median is zero, you get Inf
                                         yes = delta/0.1, # using this as a stand-in for zero for now...
                                         no = delta/site_median)))
  per_diff_min <- min(dv_stats_scaled$per_diff, na.rm = T)
  per_diff_max <- max(dv_stats_scaled$per_diff, na.rm = T)

  dv_stats_scaled2 <- dv_stats_scaled %>%
    # need to scale to be between 0-1
    mutate(delta_frac = ifelse(per_diff <= 0,
                               0.5 * (per_diff - per_diff_min) / (-per_diff_min),
                               0.5 + 0.5 * per_diff / per_diff_max)) %>%
    head()

  dv_stats_with_color <- dv_stats_scaled %>%
    mutate(shape = ifelse(is.na(delta_per),
                          no = gage_style$with_percentile$pch,
                          yes = gage_style$no_percentile$pch),
           size = ifelse(is.na(delta_per),
                         no = gage_style$with_percentile$cex,
                         yes = gage_style$no_percentile$cex),
           color = NA)

  delta_na <- is.na(dv_stats_with_color$delta_per)
  #this was not working with ifelse inside mutate, still NAs getting into rgb/col_fun
  dv_stats_with_color$color[!delta_na] <- rgb(col_fun(dv_stats_with_color$delta_per[!delta_na]), maxColorValue = 255)
  dv_stats_with_color$color[delta_na] <- color_palette$no_percentile

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_color, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}
