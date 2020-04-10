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

file.create("callouts_cgf.yml")

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
