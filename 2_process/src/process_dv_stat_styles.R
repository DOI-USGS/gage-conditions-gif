#' @title Compute the style of each point: color for each daily value percentile (reflecting anomalizes relative to day of year) and size for the typical flow on this day of year relative to flow on other days of year at the same site (reflecting typical seasonlity of flow).
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param color_palette list of colors to use for the color ramp (from viz_config.yml)
#' @param size_palette range of sizes to use to scale the circles
#' @param gage_style list indicating point type and size to use for gages with or without percentiles
#' @param normal_percentiles vector with 2 values giving the range of percentiles to treat as "normal" condition
process_dv_stat_styles <- function(ind_file, dv_stats_ind, color_palette, size_palette, gage_style, normal_percentiles){

  # read in the prepared statistics
  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))

  # widen the range for "normal"
  norm_per_low <- as.numeric(head(normal_percentiles, 1))/100
  norm_per_high <- as.numeric(tail(normal_percentiles, 1))/100
  dv_stats_adj <- dv_stats %>%
    mutate(per_adj = ifelse(per >= norm_per_low & per <= norm_per_high,
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
