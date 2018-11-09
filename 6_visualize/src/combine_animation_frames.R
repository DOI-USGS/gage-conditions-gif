combine_animation_frames <- function(gif_file, animation_cfg, task_names=NULL) {
  #build video from pngs with ffmpeg
  #note that this will use all frames in 6_visualize/tmp
  shell_command <- sprintf(
    "ffmpeg -framerate %s -start_number 20170000 -start_number_range 100000 -i 6_visualize/tmp/frame_%%08d_00.png -framerate %s -pix_fmt yuv420p %s",
    animation_cfg$frame_rate, animation_cfg$output_frame_rate, gif_file)
  system(shell_command)
}
