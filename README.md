# gage conditions


The master repo is setup to build a video file. I (Lindsay) have been running the following lines of code to build the big pieces individually. One version of the final product can be found [here](www.usgs.gov/media/videos/us-river-conditions-water-year-2018). Before running this, you need delete the old contents of `6_visualize/tmp`. 

```r
#####################
## Download data

scipiper::scmake("1_fetch/out/dv_data.rds.ind", "1_fetch.yml")
scipiper::scmake("1_fetch/out/dv_data_fixed_gh.rds.ind", "1_fetch.yml")
scipiper::scmake('1_fetch/out/sites_stage.rds.ind', remake_file = '1_fetch.yml')
scipiper::scmake("2_process/out/dv_stats.rds.ind", "2_process.yml")
scipiper::scmake("2_process/out/dv_stats_fixed_gh.rds.ind", "2_process.yml")
scipiper::scmake("2_process/out/dv_stat_styles.rds.ind", "2_process.yml")

#####################
## Create a callouts file (it can be empty if there are none)

file.create("callouts_cfg.yml")

#####################
## Build ALL frames and then make video

# To make absolutely sure that your video will use the right font, you may need to run:
sysfonts::font_add_google('Abel','abel')

scipiper::scmake('6_timestep_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
scipiper::scmake('6_visualize/log/6_timestep_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_intro_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
scipiper::scmake('6_visualize/log/6_intro_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_pause_gif_tasks.yml', remake_file = '6_visualize.yml', force=TRUE)
scipiper::scmake('6_visualize/log/6_pause_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_final_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
scipiper::scmake('6_visualize/log/6_final_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_visualize/out/year_in_review.mp4', remake_file = '6_visualize.yml', force = TRUE)

```

# Steps for testing individual frames

Sometimes it is useful to build just a single frame, or subset of frames. Below is some code that helps with that.

```r

# Build a specific subset of days
days <- c(211:215)
scipiper::scmake(sprintf('6_visualize/tmp/frame_20200%s_00.png', days), '6_timestep_gif_tasks.yml')

# Build a single frame:
scipiper::scmake('6_visualize/tmp/frame_20200210_00.png', '6_timestep_gif_tasks.yml')

```

# Steps for using script-based process for creating callouts

It is not currently built into the `scipiper` code yet. BUT here is what you need to do:

1. Once you have a basic video (see code chunk above), run the script `auto_generate_excel_file_for_review.R`. Upload this to OneDrive and share with the people who will help make the callouts.
2. Once the callouts are complete in this file. Download it and run the script `auto_callouts_cfg_from_excel_file.R`. Please note the filenames of your Excel files.
3. Now go manually edit `callouts_cfg.yml`.

Eventually, it would be nice to get this download/read/upload process into the pipeline, but for now it is not.

# To create a template for making the overlays, run the following

You can make it with or without the basemap. The important part is that it is the right dimensions.

```r
source("1_fetch/src/map_utils.R")
source("2_process/src/project_shift_states.R")
source("6_visualize/src/prep_basemap_fun.R")
source("6_visualize/src/prep_view_fun.R")
source("6_visualize/src/create_animation_frame.R")

viz_config <- yaml::yaml.load_file("viz_config.yml")
viz_config[['basemap']][["border"]] <- viz_config[['basemap']][["col"]] # No outlines on states
states_projected <- project_states('1_fetch/out/pre_state_boundaries_census.zip.ind', viz_config[['projection']])
states_shifted <- shift_states(states_projected, viz_config[['shift']])

create_animation_frame(
      png_file="6_visualize/in/overlay_template.png",
      config=viz_config[c('width','height')],
      prep_view_fun(as_view_polygon(viz_config[c('bbox', 'projection', 'width', 'height')]), viz_config['background_col']),
      prep_basemap_fun(states_shifted, viz_config[['basemap']]))
```

# To create a Drupal carousel-optimized image, run the following

```
version_info <- "river_conditions_apr_jun_2020"
frame_to_use <- "6_visualize/tmp/frame_20200401_00.png"

run_magick_cmd <- function(command_str) {
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', command_str)
  } else {
    magick_command <- command_str
  }
  system(magick_command)
}

run_magick_cmd("convert -size 11400x3721 canvas:white carousel_background.png")
run_magick_cmd(sprintf("convert -composite -gravity center carousel_background.png %s %s_carousel.png", frame_to_use, version_info))

```

# To create a USGS VisID compliant video version

```
# This works very well for viz_config height and width of 2048 & 4096.
# Unsure about what changes may be needed for other dimensions.

# Get viz frame dimensions and then divide by 2 bc we 
# double them in combine_animation_frame
timestep_frame_config <- remake::fetch("timestep_frame_config")
viz_config_dim <- lapply(timestep_frame_config, function(x) x/2) 

# Identify files
video_file <- "6_visualize/out/river_conditions_jul_sep_2020.mp4"
video_logo_cover_file <- "6_visualize/tmp/video_logocovered_for_visid.mp4"
video_scaled_for_visid_file <- "6_visualize/tmp/video_scaled_for_visid.mp4"
visid_file <- "6_visualize/in/visid_overlay.png"
video_w_visid_file <- "6_visualize/out/river_conditions_jul_sep_2020_visid.mp4"

# Cover up the existing USGS logo
system(sprintf(
  'ffmpeg -y -i %s -vf "drawbox=x=0:y=ih-h:w=%s/6:h=%s/8:t=max:color=white" %s', 
  video_file, 
  viz_config_dim$width, 
  viz_config_dim$height,
  video_logo_cover_file
))

# Scale and pad the existing video to fit the black bottom bar
# without changing aspect ratio
system(sprintf(
    'ffmpeg -y -i %s -vf "scale=%s:%s,pad=%s:%s:(ow-iw)/2:color=white" %s', 
    video_logo_cover_file,
    viz_config_dim$width-viz_config_dim$width*0.08691406, 
    viz_config_dim$height-viz_config_dim$height*0.08691406,
    viz_config_dim$width, 
    viz_config_dim$height,
    video_scaled_for_visid_file
))

# Overlay the visid black bar onto video
system(sprintf(
    'ffmpeg -y -i %s -i %s -filter_complex "overlay" -c:v libx264  %s', 
    video_scaled_for_visid_file,
    visid_file,
    video_w_visid_file))

```


# Create a visID version that isn't too big for Facebook

```
video_file <- "6_visualize/out/river_conditions_jul_sep_2020_visid.mp4"
video_resized_for_facebook <- "6_visualize/tmp/video_facebook_aspect_ratio.mp4"
video_downscaled_for_facebook <- "6_visualize/out/river_conditions_jul_sep_2020_facebook.mp4"

# Get viz frame dimensions and then divide by 2 bc we 
# double them in combine_animation_frame
timestep_frame_config <- remake::fetch("timestep_frame_config")
viz_config_dim <- lapply(timestep_frame_config, function(x) x/2) 

# need to have 16:9, not 2:1
new_height <- viz_config_dim$width * 9/16

# Scale and pad the existing video to fit 16:9 aspect ratio
#   0.8691406 is the scale factor from above for the width
#     of the logo black bar. Using it here means that we are centering
#     the image and taking that black bar into account. It's a bit
#     of a mystery to me still but it worked!
system(sprintf(
    'ffmpeg -y -i %s -vf "pad=%s:%s:0:(oh-ih)*0.8691406:color=black" %s', 
    video_file,
    viz_config_dim$width, 
    new_height,
    video_resized_for_facebook
))

# Scale down size so it doesn't upload as a 360 video
scale_factor <- 1280 / viz_config_dim$width # 1280 = optimal facebook width 

system(sprintf(
    'ffmpeg -y -i %s -vf "scale=%s:%s" %s', 
    video_resized_for_facebook,
    viz_config_dim$width * scale_factor,
    new_height * scale_factor,
    video_downscaled_for_facebook
))

```
