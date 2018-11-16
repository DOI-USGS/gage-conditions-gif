#' @title Compute the color for each daily value percentile
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
process_seasonality_colors <- function(ind_file, dv_stats_ind, color_palette, gage_style){

  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  col_fun <- colorRamp(color_palette$with_percentile)

  dv_stats_scaled <- dv_stats %>%
    group_by(site_no) %>%
    mutate(p50_quantile = ecdf(p50_va)(p50_va)) %>% # produces values between 0 and 1
    ungroup()

  dv_stats_with_color <- dv_stats_scaled %>%
    mutate(
      shape = ifelse(is.na(p50_quantile),
                     no = gage_style$with_percentile$pch,
                     yes = gage_style$no_percentile$pch),
      size = ifelse(is.na(p50_quantile),
                    no = gage_style$with_percentile$cex,
                    yes = gage_style$no_percentile$cex),
      color = ifelse(is.na(p50_quantile),
                     no = rgb(col_fun(p50_quantile), maxColorValue = 255),
                     yes = color_palette$no_percentile))

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_color, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}
