combine_animation_frames <- function(gif_file, animation_cfg, task_names=NULL) {

  if(is.null(task_names)) task_names <- '*'
  png_files <- paste(sprintf('6_visualize/tmp/frame_%s.png', task_names), collapse=' ')
  # all_files <- list.files('6_visualize/tmp', full.names = TRUE)
  # non_png_files <- all_files[!all_files %in% png_files]
  # #delete any extra files so they aren't included in the video
  # unlink(non_png_files, recursive = TRUE)
  #build video from pngs with ffmpeg
  shell_command <- sprintf(
    "ffmpeg -framerate %s -start_number 20170000 -start_number_range 100000 -i 6_visualize/tmp/frame_%%08d_00.png -framerate %s -pix_fmt yuv420p %s",
    animation_cfg$frame_rate, animation_cfg$output_frame_rate, gif_file)
  #will this just work on windows?
  system(shell_command)
}
