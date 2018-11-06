combine_animation_frames <- function(gif_file, animation_cfg, task_names=NULL) {

  # run imageMagick convert to build a gif
  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('6_visualize/tmp/frame_%s.png', task_names), collapse=' ')
  tmp_dir <- './6_visualize/tmp/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %d -loop 0 %s %s',
    tmp_dir, animation_cfg$frame_delay_cs, png_files, gif_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3
  animation_delay <- animation_cfg$frame_delay_cs
  gifsicle_command <- sprintf('gifsicle -b -O3 -d %s --colors 256 %s', animation_delay, gif_file)
  system(gifsicle_command)
}
