create_intro_frame <- function(png_file, file_config, frame_config, watermark_fun, fade) {
  if(!is.na(png_file)) {
    plot_type <- switch(Sys.info()[['sysname']],
                        Windows= "cairo",
                        Linux  = "Xlib",
                        Darwin = "quartz")
    # open the plotting device
    png(filename=png_file, width=file_config$width, height=file_config$height, units='px', type = plot_type)
  }

  # begin using google fonts
  par(family = 'abel') # may need to install from Google Fonts with sysfonts::font_add_google('Abel','abel')
  showtext_begin()

  # Setup an empty plot
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

  # It's a wonky shape
  watermark_fun()

  # Add text
  par(lheight=1.1)
  text(x = 0.5,
       y = 0.75,
       labels = frame_config$main,
       cex = 18,
       col = "#474747")
  text(x = 0.5,
       y = 0.60,
       labels = "at USGS streamgages",
       cex = 12,
       col = "#474747")
  text(x = 0.5,
       y = 0.35,
       labels = frame_config$subtitle,
       cex = 12,
       col = "#04507d")

  # close off google fonts
  showtext_end()

  # close off the plotting device
  if(!is.na(png_file)) dev.off()
}
