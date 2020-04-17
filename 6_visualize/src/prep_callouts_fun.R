
prep_callouts_fun <- function(callouts_cfg, dateTime){

  this_date <- as.POSIXct(dateTime, tz = "UTC") #watch timezones if we ever switch from daily data

  # The callouts cloud (highlighting the sites) will only appear during the event
  this_date_callouts_cloud <- lapply(callouts_cfg, function(x, this_date) {
    start <- as.POSIXct(x$text_dates$start, tz = "UTC")
    end <- as.POSIXct(x$text_dates$end, tz = "UTC")
    if(this_date >= start & this_date <= end) {
      return(x)
    } else {
      return(NULL)
    }
  }, this_date)

  # text will fade in/out before/after their actual date
  this_date_callouts_text <- lapply(callouts_cfg, function(x, this_date) {

    max_fade_in <- ifelse(is.null(x$fade_in), 9, x$fade_in) # max days before to start fading in
    max_fade_out <- ifelse(is.null(x$fade_out), 9, x$fade_out) # max days after to start fading in
    max_fade_text <- 100 # percent maximum transparency for text
    max_fade_rect <- 75 # maximum transparency for the rect

    start <- as.POSIXct(x$text_dates$start, tz = "UTC")
    end <- as.POSIXct(x$text_dates$end, tz = "UTC")

    # Figure out how many days before or after an event the current date is
    before_n <- start - this_date
    after_n <- this_date - end

    # If both before or after are negative, that means this date is during the event
    during <- all(c(before_n <= 0, after_n <= 0))

    if(before_n > 0 & before_n < max_fade_in) {
      # start fading in text n frames before the event starts
      x$text$alpha <- perc_to_hexalpha((max_fade_in-before_n)*(max_fade_text/max_fade_in))
      x$text$alpha_rect <- perc_to_hexalpha((max_fade_in-before_n)*(max_fade_rect/max_fade_in))
      return(x)
    } else if(after_n > 0 & after_n < max_fade_out) {
      # fade out the text n frames after the event ends
      x$text$alpha <- perc_to_hexalpha((max_fade_out-after_n)*(max_fade_text/max_fade_out))
      x$text$alpha_rect <- perc_to_hexalpha((max_fade_out-after_n)*(max_fade_rect/max_fade_out))
      return(x)
    } else if(during) {
      # Event text is not transparent at all durng the event
      x$text$alpha <- perc_to_hexalpha(max_fade_text)
      x$text$alpha_rect <- perc_to_hexalpha(max_fade_rect) # rectangle is slightly transparent
      return(x)
    } else {
      return(NULL)
    }
  }, this_date)

  # keep only non-NULL elements
  if(length(this_date_callouts_text) > 0) {
    this_date_callouts_cloud <- this_date_callouts_cloud[!unlist(lapply(this_date_callouts_cloud, is.null))]
    this_date_callouts_text <- this_date_callouts_text[!unlist(lapply(this_date_callouts_text, is.null))]

    n_callouts_cloud <- length(this_date_callouts_cloud)
    n_callouts_text <- length(this_date_callouts_text)
  } else {
    n_callouts_cloud <- 0
    n_callouts_text <- 0
  }

  rm(callouts_cfg, dateTime, this_date)

  if(n_callouts_text > 0) {
    # there is always text when there will be clouds, but
    # there won't always be clouds when there is text, so using n_callouts_text as the check is OK
    plot_fun <- function(){

      # it is up to the user to parse the text to make sure it doesn't end up outside of the margins
      coord_space <- par()$usr

      # iterate over all callouts that will add the background shape/cloud for this timestep
      if(n_callouts_cloud > 0) {
        for(n in 1:n_callouts_cloud) {
          this_callout_cloud <- this_date_callouts_cloud[[n]]

          # Polygon highlighting certain gages
          if('polygon' %in% names(this_callout_cloud) && 'file' %in% names(this_callout_cloud$polygon)) {
            polygon <- png::readPNG(this_callout_cloud$polygon$file)
            rasterImage(polygon, xleft=coord_space[1], ybottom=coord_space[3], xright=coord_space[2], ytop=coord_space[4])
          }
        }
      }

      # iterate over all callouts that will add the text for this timestep
      for(n in 1:n_callouts_text) {
        this_callout <- this_date_callouts_text[[n]]

        # Prep for adding the text and text box
        add_box <- ifelse(!is.null(this_callout$add_box),
                          this_callout$add_box, FALSE)
        callout_text_cfg_n <- this_callout$text
        x <- coord_space[1] + callout_text_cfg_n$x_loc * diff(coord_space[1:2])
        y <- coord_space[3] + callout_text_cfg_n$y_loc * diff(coord_space[3:4])
        callout_text_lines <- callout_text_cfg_n$label
        font_x_multiplier <- 4.1 # for Abel w cex=4
        font_y_multiplier <- 5 # for Abel w cex=4
        y_bot <- y - (length(callout_text_lines)-1)*strheight(callout_text_lines[1])*font_y_multiplier

        # Add the box behind the text if applicable
        if(add_box) {
          # font_multipliers are buffers for abel since strwidth can't do abel
          # font sizes correctly
          max_strwidth <- max(strwidth(callout_text_lines))*font_x_multiplier
          max_strheight <- max(strheight(callout_text_lines))*font_y_multiplier

          x_buffer_left <- switch(
            as.character(callout_text_cfg_n$pos),
            "NULL" = max_strwidth/2, # centered
            "2" = max_strwidth, # left of
            "4" = 0, # right of
            max_strwidth/2 # default is centered
          )
          x_buffer_right <- switch(
            as.character(callout_text_cfg_n$pos),
            "NULL" = max_strwidth/2, # centered
            "2" = 0, # left of
            "4" = max_strwidth, # right of
            max_strwidth/2 # default is centered
          )
          y_buffer_top <- switch(
            as.character(callout_text_cfg_n$pos),
            "NULL" = 0,
            "1" = 0, # below
            "3" = max_strheight, # above
            max_strheight*0.6 # default
          )
          y_buffer_bottom <- switch(
            as.character(callout_text_cfg_n$pos),
            "NULL" = 0,
            "1" = max_strheight, # below is the default position for text
            "3" = 0, # above
            max_strheight*0.5 # default
          )
          rect(xleft = x - x_buffer_left,
               xright = x + x_buffer_right,
               ybottom = y_bot - y_buffer_bottom,
               ytop = y + y_buffer_top,
               col = paste0("#aaaaaa", callout_text_cfg_n$alpha_rect), border = NA)
          # rect_left <- x - x_buffer_left
          # rect_right <- x + x_buffer_right
          # rect_center_x <- (rect_left + rect_right)/2
          # rect_bot <- y_bot - y_buffer_bottom
          # rect_top <- y + y_buffer_top
          # rect_center_y <- (rect_bot + rect_top)/2
          # plot_width <- coord_space[2]-coord_space[1]
          # plot_height <- coord_space[4]-coord_space[3]
          # plot_left <- coord_space[1]
          # plot_bot <- coord_space[3]
          # grid::grid.roundrect(
          #   x = (rect_center_x - plot_left) / plot_width,
          #   y = (rect_center_y - plot_bot) / plot_height,
          #   just = 'centre',
          #   width = 1.04 * (rect_right - rect_left) / plot_width,
          #   height = 1.04 * (rect_top - rect_bot) / plot_height,
          #   r = unit(20, 'points'),
          #   gp = gpar(
          #     fill = '#bdbdbd5E',
          #     col = NA))

        }

        # Enable text centering
        if(callout_text_cfg_n$pos == "center") {
          pos <- NULL
          adj <- 0.5
        } else {
          pos <- callout_text_cfg_n$pos
          adj <- NULL
        }

        # Add the text
        for (i in 1:length(callout_text_lines)) {
          y_i <- y - (i-1)*strheight(callout_text_lines[i])*font_y_multiplier
          text(x, y_i, labels = callout_text_lines[i],
               cex = callout_text_cfg_n$cex,
               adj = adj,
               pos = pos,
               col = paste0("#383838", callout_text_cfg_n$alpha))
        }
      }
    }

  } else {
    rm(this_date_callouts_cloud, this_date_callouts_text, n_callouts_text, n_callouts_cloud)
    plot_fun <- function() { NULL }
  }
  return(plot_fun)
}

perc_to_hexalpha <- function(perc_in) {
  fade_perc <- c(0, 10, 20, 30, 35, 40, 50, 60, 70, 80, 90, 100, Inf)
  fade_alpha <- c("00", "1A", "33", "4D", "59", "66", "80", "99", "B3", "CC", "E6", "FF")
  return(fade_alpha[findInterval(perc_in, fade_perc)])
}
