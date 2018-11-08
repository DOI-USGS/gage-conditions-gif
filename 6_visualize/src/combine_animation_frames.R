combine_animation_frames <- function(gif_file, animation_cfg, task_names=NULL) {

  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('6_visualize/tmp/frame_%s.png', task_names), collapse=' ')
  #build video from pngs with ffmpeg
  shell_command <- sprintf(
    "ffmpeg -framerate %s -pattern_type glob -i '6_visualize/tmp/frame_*.png' %s", animation_cfg$frame_rate, gif_file)
  #will this just work on windows?
  system(shell_command)
}
