#' @title Compute the color for each daily value percentile
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
process_dv_stat_colors <- function(ind_file, dv_stats_ind, color_palette, gage_style){

  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))

  # widen the range for "normal"
  dv_stats_adj <- dv_stats %>%
    mutate(per_adj = ifelse(per >= 0.2 & per <= 0.8,
                            no = per, yes = 0.5))

  col_fun <- colorRamp(color_palette$with_percentile)
  dv_stats_with_color <- dv_stats_adj %>%
    mutate(shape = ifelse(is.na(per_adj), no = gage_style$with_percentile$pch,
                         yes = gage_style$no_percentile$pch),
          size = ifelse(is.na(per_adj), no = gage_style$with_percentile$cex,
                        yes = gage_style$no_percentile$cex),
          color = NA)
  per_na <- is.na(dv_stats_adj$per_adj)

  #this was not working with ifelse inside mutate, still NAs getting into rgb/col_fun
  dv_stats_with_color$color[!per_na] <- rgb(col_fun(dv_stats_adj$per_adj[!per_na]), maxColorValue = 255)
  dv_stats_with_color$color[per_na] <- color_palette$no_percentile

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_color, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}
