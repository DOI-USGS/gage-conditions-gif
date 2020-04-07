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

scipiper::scmake('6_timestep_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
scipiper::scmake('6_visualize/log/6_timestep_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_pause_gif_tasks.yml', remake_file = '6_visualize.yml', force=TRUE)
scipiper::scmake('6_visualize/log/6_pause_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_final_gif_tasks.yml', remake_file = '6_visualize.yml', force = TRUE)
scipiper::scmake('6_visualize/log/6_final_gif_tasks.ind', remake_file = '6_visualize.yml', force=TRUE)

scipiper::scmake('6_visualize/out/year_in_review.mp4', remake_file = '6_visualize.yml', force = TRUE)
```

