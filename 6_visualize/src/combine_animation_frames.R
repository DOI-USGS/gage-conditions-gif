combine_animation_frames <- function(gif_file, animation_cfg) {
  #build video from pngs with ffmpeg
  #note that this will use all frames in 6_visualize/tmp
  #have to rename files since can't use globbing with ffmpeg on Windows :(
  png_frames <- list.files('6_visualize/tmp', full.names = TRUE)
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = zeroPad(1:length(png_frames), padTo = 3),
                         newName = file.path("6_visualize/tmp", paste0("frame_", countFormatted, ".png")))
  file.rename(from = file_name_df$origName, to = file_name_df$newName)
  shell_command <- sprintf(
    "ffmpeg -y -framerate %s -i 6_visualize/tmp/frame_%%03d.png -framerate %s -pix_fmt yuv420p %s",
    animation_cfg$frame_rate, animation_cfg$output_frame_rate, gif_file)
  system(shell_command)
  file.rename(from = file_name_df$newName, to = file_name_df$origName)
}
