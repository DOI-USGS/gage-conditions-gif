#' @title Compute the style of each point: color for each daily value percentile (reflecting anomalizes relative to day of year) and size for the typical flow on this day of year relative to flow on other days of year at the same site (reflecting typical seasonlity of flow).
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_stats_ind indicator file for the data.frame of dv_data
#' @param stage_ind indicator file for the data.frame of sites with flood stage
#' @param gage_style list indicating point type and size to use for gages with or without percentiles
#' @param display_percentiles list with percentiles for normal_range, drought_severe, drought_low, and flood
process_dv_stat_styles <- function(ind_file, dv_stats_ind, stage_ind, gage_style, display_percentiles){

  # read in the prepared statistics
  dv_stats <- readRDS(scipiper::sc_retrieve(dv_stats_ind, remake_file = '2_process.yml'))
  display_percentiles_num <- lapply(display_percentiles, function(x) as.numeric(x)/100)

  sites_stage <- readRDS(scipiper::sc_retrieve(stage_ind, remake_file = '2_process.yml'))
  dv_stats <- dv_stats %>% left_join(sites_stage, by = "site_no")

  # widen the range for "normal"
  norm_per_low <- min(display_percentiles_num$normal_range)
  norm_per_high <- max(display_percentiles_num$normal_range)
  dv_stats_adj <- dv_stats %>%
    mutate(per = ifelse(per >= norm_per_low & per <= norm_per_high,
                            no = per, yes = 0.5))

  # set the styles
  dv_stats_with_style <- dv_stats_adj %>%
    add_style_columns(gage_style, display_percentiles_num)

  # Write the data file and the indicator file
  saveRDS(dv_stats_with_style, scipiper::as_data_file(ind_file))
  scipiper::gd_put(ind_file)
}

add_style_columns <- function(per_df, gage_style, percentiles) {
  mutate(per_df,
    condition = case_when(
      !is.na(dv_val) & !is.na(flood_stage) & dv_val >= flood_stage ~ "Flooding",
      per >= percentiles$wet ~ "Wettest",
      per >= max(percentiles$normal_range) & per < percentiles$wet ~ "Wet",
      per > min(percentiles$normal_range) & per < max(percentiles$normal_range) ~ "Normal",
      per > percentiles$drought_low & per <= min(percentiles$normal_range) ~ "Dry",
      per > percentiles$drought_severe & per <= percentiles$drought_low ~ "Drier",
      per <= percentiles$drought_severe ~ "Driest",
      is.na(per) ~ "No data"
    )
  ) %>% mutate(

    bg = case_when(
      condition == "Flooding" ~ gage_style$with_percentile$flood$bg,
      condition == "Wettest" ~ gage_style$with_percentile$wet$bg[2],
      condition == "Wet" ~ gage_style$with_percentile$wet$bg[1],
      condition == "Normal" ~ gage_style$with_percentile$normal$bg,
      condition %in% c("No data", "Dry", "Drier", "Driest") ~ NA_character_
    ),

    pch = case_when(
      condition == "Flooding" ~ gage_style$with_percentile$flood$pch,
      condition %in% c("Wet", "Wettest") ~ gage_style$with_percentile$wet$pch,
      condition == "Normal" ~ gage_style$with_percentile$normal$pch,
      condition %in% c("Dry", "Drier", "Driest") ~ gage_style$with_percentile$drought$pch,
      condition == "No data" ~ gage_style$no_percentile$pch
    ),

    cex = case_when(
      condition == "Flooding" ~ gage_style$with_percentile$flood$cex,
      condition %in% c("Wet", "Wettest") ~ gage_style$with_percentile$wet$cex,
      condition == "Normal" ~ gage_style$with_percentile$normal$cex,
      condition == "Dry" ~ gage_style$with_percentile$drought$cex[1],
      condition == "Drier" ~ gage_style$with_percentile$drought$cex[2],
      condition == "Driest" ~ gage_style$with_percentile$drought$cex[3],
      condition == "No data" ~ gage_style$no_percentile$cex
    ),

    col = case_when(
      condition == "Flooding" ~ gage_style$with_percentile$flood$col,
      condition %in% c("Normal", "Wet", "Wettest") ~ NA_character_,
      condition == "Dry" ~ gage_style$with_percentile$drought$col[1],
      condition == "Drier" ~ gage_style$with_percentile$drought$col[2],
      condition == "Driest" ~ gage_style$with_percentile$drought$col[3],
      condition == "No data" ~ gage_style$no_percentile$col
    ),

    lwd = case_when(
      condition == "Flooding" ~ gage_style$with_percentile$flood$lwd,
      TRUE ~ 1.0 # Everything else
    )
  )
}
