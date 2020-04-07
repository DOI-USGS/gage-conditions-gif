combine_animation_frames_video <- function(out_file, animation_cfg) {
  #build video from pngs with ffmpeg
  #note that this will use all frames in 6_visualize/tmp
  #have to rename files since can't use globbing with ffmpeg on Windows :(
  png_frames <- list.files('6_visualize/tmp', full.names = TRUE)
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = zeroPad(1:length(png_frames), padTo = 3),
                         newName = file.path("6_visualize/tmp", paste0("frame_", countFormatted, ".png")))
  file.rename(from = file_name_df$origName, to = file_name_df$newName)

  # added ffmpeg better code for reducing video size
    # see https://unix.stackexchange.com/questions/28803/how-can-i-reduce-a-videos-size-with-ffmpeg
    # and https://slhck.info/video/2017/02/24/crf-guide.html

  shell_command <- sprintf(
    "ffmpeg -y -framerate %s -i 6_visualize/tmp/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
    animation_cfg$frame_rate, animation_cfg$output_frame_rate, out_file)
  system(shell_command)

  file.rename(from = file_name_df$newName, to = file_name_df$origName)
}

combine_animation_frames_gif <- function(out_file, animation_cfg) {
  #build gif from pngs with magick and simplify with gifsicle
  #note that this will use all frames in 6_visualize/tmp
  png_files <- paste(list.files('6_visualize/tmp', full.names = TRUE), collapse=' ')
  tmp_dir <- '6_visualize/tmp/magick'
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)

  # create gif using magick
  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %d -loop 0 %s %s',
    tmp_dir, animation_cfg$frame_delay_cs, png_files, out_file)
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  system(magick_command)

  # simplify the gif with gifsicle - cuts size by about 2/3
  gifsicle_command <- sprintf('gifsicle -b -O3 -d %s --colors 256 %s',
                              animation_cfg$frame_delay_cs, out_file)
  system(gifsicle_command)
}
