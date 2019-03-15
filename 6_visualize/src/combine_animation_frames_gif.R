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
