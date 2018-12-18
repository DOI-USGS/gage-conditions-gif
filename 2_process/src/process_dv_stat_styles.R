#' @title Compute the style of each point: color for each daily value percentile (reflecting anomalizes relative to day of year) and size for the typical flow on this day of year relative to flow on other days of year at the same site (reflecting typical seasonlity of flow).
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
process_dv_stat_styles <- function(ind_file, dv_stats_ind, color_palette, size_palette, gage_style){

  # read in the prepared statistics
  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))

  # widen the range for "normal"
  dv_stats_adj <- dv_stats %>%
    mutate(per_adj = ifelse(per >= 0.25 & per <= 0.75,
                            no = per, yes = 0.5))

  # define the styling functions
  col_fun <- colorRamp(color_palette$with_percentile)
  size_fun <- function(percentile) {
    # to use constant dot size:
    gage_style$with_percentile$cex

    # to show seasonality through dot size:
    #size_palette$cex_range[1] + percentile*(diff(size_palette$cex_range))
  }

  # apply the styling functions to add style columns to the data.frame
  dv_stats_with_style <- dv_stats_adj %>%
    mutate(
      shape = ifelse(
        is.na(per_adj),
        no = gage_style$with_percentile$pch,
        yes = gage_style$no_percentile$pch),
      size = ifelse(
        is.na(per_adj),
        no = size_fun(p50_quantile),
        yes = gage_style$no_percentile$cex),
      color = NA)

  # insert values into the color column. this was not working with ifelse inside
  # mutate, still NAs getting into rgb/col_fun
  per_na <- is.na(dv_stats_with_style$per_adj)
  dv_stats_with_style$color[!per_na] <- rgb(col_fun(dv_stats_with_style$per_adj[!per_na]), maxColorValue = 255)
  dv_stats_with_style$color[per_na] <- color_palette$no_percentile

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_style, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}
