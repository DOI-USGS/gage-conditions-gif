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

# Build a frame for the middle of each event

dates_to_build <- lapply(lapply(yaml::read_yaml("callouts_cfg.yml"), '[[', "text_dates"), function(x) {
  endDate <- as.Date(x$end)
  startDate <- as.Date(x$start)
  halfwayDate <- startDate + (endDate - startDate)/2
  return(halfwayDate)
}) %>% unlist() %>% as.Date(origin = "1970-01-01") %>% format("%Y%m%d")

scipiper::scmake(sprintf('6_visualize/tmp/frame_%s_00.png', dates_to_build), '6_timestep_gif_tasks.yml')


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
version_info <- "river_conditions_jul_sep_2020"
frame_to_use <- "6_visualize/tmp/frame_20200924_00.png"

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

# To create a Drupal thumbnail-optimized image, run the following

```
version_info <- "river_conditions_jul_sep_2020"
frame_to_use <- "6_visualize/tmp/frame_20200929_00.png"
thumbnail_dim <- 500

viz_config <- yaml::yaml.load_file("viz_config.yml")
width <- viz_config[["width"]]
height <- viz_config[["height"]]
x_pos <- viz_config[["footnote_cfg"]][["x_pos"]]
y_pos <- viz_config[["footnote_cfg"]][["y_pos"]]

run_magick_cmd <- function(command_str) {
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', command_str)
  } else {
    magick_command <- command_str
  }
  system(magick_command)
}

# Crop frame to map only view & resize so that width is 500
run_magick_cmd(sprintf("convert %s -gravity West -chop %sx0 -gravity South -chop 0x%s -resize %sx%s drupal_thumbnail_intermediate.png", frame_to_use, width*x_pos*0.80, height*y_pos*2, thumbnail_dim, thumbnail_dim))

# Create a square white image
run_magick_cmd(sprintf("convert -size %sx%s canvas:white drupal_thumbnail.png", thumbnail_dim, thumbnail_dim))

# Paste the map centered in the square white image
run_magick_cmd(sprintf("convert -composite -gravity center drupal_thumbnail.png drupal_thumbnail_intermediate.png %s_thumbnail.png", version_info))
      
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
video_file <- "6_visualize/out/river_conditions_jul_sep_2020_largetext_twitter.mp4"
video_logo_cover_file <- "6_visualize/tmp/video_logocovered_for_visid.mp4"
video_scaled_for_visid_file <- "6_visualize/tmp/video_scaled_for_visid.mp4"
visid_file <- "6_visualize/in/visid_overlay.png"
video_w_visid_file <- "6_visualize/out/river_conditions_jul_sep_2020_visid_largetext.mp4"

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
video_file <- "6_visualize/out/river_conditions_jul_sep_2020_visid_largetext.mp4"
video_resized_for_facebook <- "6_visualize/tmp/video_facebook_aspect_ratio.mp4"
video_downscaled_for_facebook <- "6_visualize/out/river_conditions_jul_sep_2020_largetext_facebook.mp4"

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

# Create an Instagram square version by cutting, pasting, and moving datewheel, title, and legend

```
video_file <- "6_visualize/out/river_conditions_jul_sep_2020_largetext_twitter.mp4"
video_map_only <- "6_visualize/tmp/map_only.mp4"
video_map_square <- "6_visualize/tmp/map_square.mp4"
video_datewheel <- "6_visualize/tmp/datewheel.mp4"
video_legend <- "6_visualize/tmp/legend.mp4"
video_title <- "6_visualize/tmp/title.mp4"
video_logo <- "6_visualize/tmp/logo.mp4"
video_stitched <- "6_visualize/tmp/stitched.mp4"
video_intro <- "6_visualize/tmp/intro.mp4"
video_outro <- "6_visualize/tmp/outro.mp4"
video_insta <- "6_visualize/out/river_conditions_jul_sep_2020_largetext_insta.mp4"

reg_animation_start <- 4 # seconds into animation that map is first shown
reg_animation_end <- 49 # seconds into animation that map is last shown

insta_dim <- 600 # square shape

viz_config <- yaml::yaml.load_file("viz_config.yml")
width <- viz_config[["width"]]/2
height <- viz_config[["height"]]/2

## Crop video to create a map version

# Find edge of map
x_pos <- viz_config[["footnote_cfg"]][["x_pos"]]
map_info_cutoff <- width*x_pos*0.90

# Now crop to map
system(sprintf(
  'ffmpeg -y -i %s -vf "crop=%s:%s:%s:%s" %s', 
  video_file,
  width - map_info_cutoff, 
  height,
  map_info_cutoff,
  0,
  video_map_only
))

## Make map video a square with white space on top by
#   increasing height to be the same as width
system(sprintf(
  'ffmpeg -y -i %s -vf "pad=iw:iw:0:(oh-ih):color=white" %s', 
  video_map_only,
  video_map_square
))

## Create a video that contains only the datewheel

# Find wheel location
wheel_radius <- viz_config[["datewheel_cfg"]][["wheel_per"]]*width/2
wheel_center_x <- viz_config[["datewheel_cfg"]][["x_pos"]]*width
wheel_center_y <- viz_config[["datewheel_cfg"]][["y_pos"]]*height

# Now crop out just wheel
buffer<-1.2
system(sprintf(
  'ffmpeg -y -i %s -vf "crop=%s:%s:%s:%s" %s', 
  video_file,
  wheel_radius*2*buffer, # diameter of wheel 
  wheel_radius*2*buffer,
  wheel_center_x - wheel_radius*buffer,
  wheel_center_y + wheel_radius,
  video_datewheel
))

## Create a video that contains only the legend

# Find legend location
legend_guess_width <- 0.08*width #10% width of video
legend_guess_height <- 0.32*height #30% height of video
legend_x <- viz_config[["legend_cfg"]][["x_pos"]]*width
legend_y <- viz_config[["legend_cfg"]][["y_pos"]]*height

# Now crop out just legend and scale to be bigger
system(sprintf(
  'ffmpeg -y -i %s -vf "crop=%s:%s:%s:%s" %s', 
  video_file,
  legend_guess_width,  
  legend_guess_height,
  legend_x - legend_guess_width/1.5,
  height - legend_y*1.05,
  video_legend
))

## Create a video that contains only the title

# Find title location
title_guess_width <- wheel_radius*2*1.8 # diameter of wheel + some
title_guess_height <- 0.20*height #20% height of video
title_x <- viz_config[["title_cfg"]][["x_pos"]]*width
title_y <- viz_config[["title_cfg"]][["y_pos"]]*height

# Now crop out just title
system(sprintf(
  'ffmpeg -y -i %s -vf "crop=%s:%s:%s:%s" %s', # scale=%s:-1
  video_file,
  title_guess_width,  
  title_guess_height,
  0,
  0,
  # width/3, # Scale to fit 1/3 of the final video
  video_title
))

# Create a video that contains only the logo

# Find logo location
logo_guess_width <- wheel_radius*2*1.2 # diameter of wheel
logo_guess_height <- 0.10*height #10% height of video
logo_x <- 0
logo_y <- 0

# Now crop out just title
system(sprintf(
  'ffmpeg -y -i %s -vf "crop=%s:%s:%s:%s" %s', # scale=%s:-1
  video_file,
  logo_guess_width,  
  logo_guess_height,
  0,
  height,
  video_logo
))

# Overlay these videos on top of existing video
# And cut out intro & outro (reg animation starts at 4 seconds, ends at 45)
system(sprintf(
  'ffmpeg -y -i %s -i %s -i %s -i %s -i %s -filter_complex "overlay=%s:%s,overlay=%s:%s,overlay=%s:%s,overlay=%s:%s,scale=%s:-1" -ss 00:00:%s  -t 00:00:%s %s', 
  video_map_square,
  video_datewheel,
  video_legend,
  video_title,
  video_logo,
  # Add date wheel
  "(W-(W/3))",# Center in right half
  sprintf("(H-h-%s)/2", height), # Center in white space above map
  # Add legend
  "(W/2)-(w/2)", # Center 
  sprintf("(H-h-%s)/2", height), # Center in white space above map
  # Add title
  "(W*0.05)",# Just in from the left
  sprintf("(H-%s)/2", height), # Center in white space above map
  # Add logo
  "(W*0.05)",# Just in from the left
  sprintf("(H-%s)/2-%s", height, legend_guess_height/3), # Center in white space above map #""
  insta_dim,
  sprintf("%02d", reg_animation_start), # start animation
  sprintf("%02d", reg_animation_end-reg_animation_start), # end animation
  video_stitched
))


# Need intro text centered
# So cutting video and then adding in at the beginning
system(sprintf(
  'ffmpeg -y -i %s -ss 00:00:00 -t 00:00:%s -vf "crop=iw:(ih-%s):0:0,pad=iw:iw:(ow-iw)/2:(oh-ih)/2:color=white,scale=%s:-1" %s', # scale=%s:-1
  video_file,
  sprintf("%02d", reg_animation_start-1),
  logo_guess_height,  
  insta_dim,
  video_intro
))

# Now do the same thing to the outro
system(sprintf(
  'ffmpeg -y -i %s -ss 00:00:%s -vf "crop=iw:(ih-%s):0:0,pad=iw:iw:(ow-iw)/2:(oh-ih)/2:color=white,scale=%s:-1" %s', # scale=%s:-1
  video_file,
  sprintf("%02d", reg_animation_end+1),
  logo_guess_height,  
  insta_dim,
  video_outro
))

# Bring them all together
files_to_cat_fn <- "6_visualize/tmp/videos_to_concat.txt"
writeLines(sprintf("file '%s'", c(basename(video_intro), basename(video_stitched), basename(video_outro))), files_to_cat_fn)

system(sprintf(
  'ffmpeg -y -safe 0 -f concat -i %s -c copy %s',
  files_to_cat_fn,
  video_insta
))

```
