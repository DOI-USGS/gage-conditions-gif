#' @title Compute the color for each daily value percentile
#' 
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
process_dv_stat_colors <- function(ind_file, dv_stats_ind, color_palette){
  
  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  col_fun <- colorRamp(color_palette)
  
  # just removing NA percentiles for now
  dv_stats_with_color <- dv_stats %>% 
    filter(!is.na(per)) %>% 
    mutate(color = rgb(col_fun(per), maxColorValue = 255)) # don't know how necessary maxColorValue is
  
  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(dv_stats_with_color, data_file)
  scipiper::gd_put(ind_file, data_file)
}
