create_final_frame <- function(png_file, file_config, frame_config) {
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

  # Add text
  n_paragraphs <- length(frame_config)
  for(n in 1:n_paragraphs) {
    par(lheight=frame_config[[n]]$lheight)
    text(x = 0.5, y = frame_config[[n]]$ypos,
         labels = paste(frame_config[[n]]$text, collapse = ""),
         cex = frame_config[[n]]$cex, col = frame_config[[n]]$col)
  }


  # close off google fonts
  showtext_end()

  # close off the plotting device
  if(!is.na(png_file)) dev.off()
}
