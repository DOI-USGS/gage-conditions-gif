# Helper functions when building or iterating on the data, frames, and video

#' Function to re-download and re-process gage data
#'
#' @description This wraps `scipiper::scmake()` calls to rebuild different
#' data that are needed by the animation. This should be rerun after the dates
#' have been updated in `viz_config.yml`.
#'
rebuild_gage_data <- function() {
  # TODO: I think that there is a way to do this without specifying all of
  # these individually, but this has been working for me so let's save that
  # change for a rainy day.

  scipiper::scmake("1_fetch/out/dv_data.rds.ind", "1_fetch.yml")
  scipiper::scmake("1_fetch/out/dv_data_fixed_gh.rds.ind", "1_fetch.yml")
  scipiper::scmake("1_fetch/out/sites_stage.rds.ind", remake_file = "1_fetch.yml")
  scipiper::scmake("2_process/out/dv_stats.rds.ind", "2_process.yml")
  scipiper::scmake("2_process/out/dv_stats_fixed_gh.rds.ind", "2_process.yml")
  scipiper::scmake("2_process/out/dv_stat_styles.rds.ind", "2_process.yml")
}

#' Function to rebuild any or all frames
#'
#' @description This wraps `scipiper::scmake()` calls to rebuild frames. Frame sections are
#' automatically set to NOT rebuild, but any or all can be turned from `FALSE` to `TRUE` to
#' force a rebuild. The font is loaded before any frames are rebuilt.
#'
#' @param intro logical to say whether the `intro` frames should be force rebuilt
#' @param timestep logical to say whether the `timestep` frames should be force rebuilt
#' @param pause logical to say whether the `pause` frames should be force rebuilt
#' @param final logical to say whether the `final` frames should be force rebuilt
#'
rebuild_frame_sections <- function(intro = FALSE, timestep = FALSE, pause = FALSE, final = FALSE) {
  # Load that font again just in case! It's easy and quick to run but terrible
  # to realize your fonts are all wrong after the frames rebuild
  sysfonts::font_add_google('Abel','abel')

  if(intro) {
    # Build the intro frames
    scipiper::scmake('6_intro_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
    scipiper::scmake('6_visualize/log/6_intro_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)
  }

  if(timestep) {
    # Build the timestep frames
    scipiper::scmake('6_timestep_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
    scipiper::scmake('6_visualize/log/6_timestep_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)
  }

  if(pause) {
    # Build the pause frames
    scipiper::scmake('6_pause_gif_tasks.yml', remake_file = '6_visualize.yml', force=TRUE)
    scipiper::scmake('6_visualize/log/6_pause_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)
  }

  if(final) {
    # Build the final frames
    scipiper::scmake('6_final_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
    scipiper::scmake('6_visualize/log/6_final_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)
  }

}

#' Function to rebuild the video
#'
#' @description This wraps a `scipiper::scmake()` call to rebuild the video and includes
#' an option to rename the output file.
#'
#' @param new_filename filepath to use to rename the output file, defaults to NULL so the
#' file would not be renamed and instead left as `6_visualize/out/year_in_review.mp4`
#'
rebuild_video <- function(new_filename = NULL) {
  # Build the animation
  scipiper::scmake('6_visualize/out/year_in_review.mp4', remake_file = '6_visualize.yml', force = TRUE)

  if(!is.null(new_name)) {
    file.rename(from = '6_visualize/out/year_in_review.mp4', to = new_name)
  }
}

#' Function to rebuild specific timestep frames
#'
#' @description This wraps `scipiper::scmake()` calls to rebuild the specified timestep frames
#'
#' @param days vector representing the day(s) that should be rebuilt
#'
rebuild_timestep_frames <- function(days) {

  # Load the font
  sysfonts::font_add_google('Abel','abel')

  # Build frames for the day(s) specified
  scipiper::scmake(sprintf('6_visualize/tmp/frame_%s_00.png', days), '6_timestep_gif_tasks.yml')

}

#' Function to rebuild a frame per event
#'
#' @description This wraps `scipiper::scmake()` calls to rebuild a
#' frame for each event by identifying the center date of the event
#' and then calling `rebuild_timestep_frames()`
#'
#' @param days vector representing the day(s) that should be rebuilt
#'
rebuild_event_frames <- function() {
  library(dplyr)
  dates_to_build <- lapply(lapply(yaml::read_yaml("callouts_cfg.yml"), '[[', "text_dates"), function(x) {
    endDate <- as.Date(x$end)
    startDate <- as.Date(x$start)
    halfwayDate <- startDate + (endDate - startDate)/2
    return(halfwayDate)
  }) %>% unlist() %>% as.Date(origin = "1970-01-01") %>% format("%Y%m%d")

  rebuild_timestep_frames(dates_to_build)
}

#' Function to graph the event durations
#'
#' @description This creates a graph to show all the events and when they
#' start or stop based on the information in `callouts_cfg.yml`. Just re-run
#' this function if you update the information in `callouts_cfg.yml`.
#'
generate_event_graph <- function() {
  library(ggplot2)

  dates_of_events <- lapply(yaml::read_yaml("callouts_cfg.yml"), function(x) {
    tibble(label = paste(x$text$label, collapse = " "),
           start = as.Date(x$event_dates$start), end = as.Date(x$event_dates$end),
           txt_s = as.Date(x$text_dates$start), txt_e = as.Date(x$text_dates$end)) %>%
      mutate(
        txt_in = txt_s - ifelse(is.null(x$fade_in), 9, x$fade_in),
        txt_out = txt_e + ifelse(is.null(x$fade_out), 9, x$fade_out)
      )
  }) %>% bind_rows() %>%
    # Order the figure output based on event start date
    arrange(start) %>%
    mutate(label = factor(label, levels = label, ordered = TRUE))

  ggplot(dates_of_events, aes(y = 1, yend = 1)) +
    geom_segment(aes(x = start, xend = end), size = 3) +
    geom_segment(aes(y = 0.5, yend = 0.5, x = txt_s, xend = txt_e), size = 2, color = "blue") +
    geom_segment(aes(y = 0.5, yend = 0.5, x = txt_in, xend = txt_out), size = 1, color = "red", linetype = "dotted") +
    ylim(0, 2) +
    geom_text(aes(x = start, y = 1.5, label = label), hjust = 0) +
    facet_grid(label ~ .) +
    theme(axis.text=element_blank(), axis.ticks=element_blank(),
          strip.background = element_blank(), strip.text = element_blank(),
          axis.title = element_blank(), panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"))
}

#' Function to create the `callouts_cfg.yml`
#'
#' @description This uses a table of callout events and the template
#' `1_fetch/in/callout_template.mustache` to generate the required
#' `callouts_cfg.yml` used by the animation code.
#'
#' @param input_xlsx filepath to an XLSX file with three columns: `Start`,
#' `End`, and `Label`. Defaults to `input_callouts.xlsx`.
#'
generate_callout_cfg_from_xlsx <- function(input_xlsx = "input_callouts.xlsx") {
  library(dplyr)

  callout_data <- readxl::read_excel(input_xlsx) %>%
    # Make columns match what the mustache template expects
    rename(start_date = Start,
           end_date = End,
           label = Label)

  # Turn data frame into list
  callouts_list <- split(callout_data, seq(nrow(callout_data)))
  callouts_string_list <- lapply(callouts_list, function(x) {
    callout_list_t <- t(x)
    callout_list_data <- setNames(split(callout_list_t, seq(nrow(callout_list_t))), rownames(callout_list_t))
    whisker::whisker.render(readLines("1_fetch/in/callout_template.mustache"), data = callout_list_data)
  })

  # Save output as file
  writeLines(unlist(lapply(callouts_string_list, paste, collapse=" ")), "callouts_cfg.yml")
}
