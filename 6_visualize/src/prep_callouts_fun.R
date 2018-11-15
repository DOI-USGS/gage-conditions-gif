
prep_callouts_fun <- function(callouts_cfg, dateTime){

  this_date <- as.POSIXct(dateTime, tz = "UTC") #watch timezones if we ever switch from daily data
  this_date_callouts <- lapply(callouts_cfg, function(x, this_date) {
    start <- as.POSIXct(x$dates$start, tz = "UTC")
    end <- as.POSIXct(x$dates$end, tz = "UTC")
    if(this_date >= start & this_date <= end) {
      return(x)
    } else {
      return(NULL)
    }
  }, this_date)

  # keep only non-NULL elements
  this_date_callouts <- this_date_callouts[!unlist(lapply(this_date_callouts, is.null))]

  rm(callouts_cfg, dateTime, this_date)

  n_callouts <- length(this_date_callouts)
  if(n_callouts > 0) {
    plot_fun <- function(){

      # it is up to the user to parse the text to make sure it doesn't end up outside of the margins
      coord_space <- par()$usr

      # iterate over all callouts that apply to this timestep
      for(n in 1:n_callouts) {
        this_callout <- this_date_callouts[[n]]

        # Polygon highlighting certain gages
        if('polygon' %in% names(this_callout) && 'file' %in% names(this_callout$polygon)) {
          polygon <- png::readPNG(this_callout$polygon$file)
          rasterImage(polygon, xleft=coord_space[1], ybottom=coord_space[3], xright=coord_space[2], ytop=coord_space[4])
        }

        # Prep for adding the text and text box
        add_box <- ifelse(!is.null(this_callout$add_box),
                          this_callout$add_box, FALSE)
        callout_text_cfg_n <- this_callout$text
        x <- coord_space[1] + callout_text_cfg_n$x_loc * diff(coord_space[1:2])
        y <- coord_space[3] + callout_text_cfg_n$y_loc * diff(coord_space[3:4])
        callout_text_lines <- callout_text_cfg_n$label
        font_x_multiplier <- 2.1 # for Abel
        font_y_multiplier <- 3 # for Abel
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
          # rect(xleft = x - x_buffer_left,
          #      xright = x + x_buffer_right,
          #      ybottom = y_bot - y_buffer_bottom,
          #      ytop = y + y_buffer_top,
          #      col = "#bdbdbd5E", border = NA)
          rect_left <- x - x_buffer_left
          rect_right <- x + x_buffer_right
          rect_center_x <- (rect_left + rect_right)/2
          rect_bot <- y_bot - y_buffer_bottom
          rect_top <- y + y_buffer_top
          rect_center_y <- (rect_bot + rect_top)/2
          plot_width <- coord_space[2]-coord_space[1]
          plot_height <- coord_space[4]-coord_space[3]
          plot_left <- coord_space[1]
          plot_bot <- coord_space[3]
          grid::grid.roundrect(
            x = (rect_center_x - plot_left) / plot_width,
            y = (rect_center_y - plot_bot) / plot_height,
            just = 'centre',
            width = 1.04 * (rect_right - rect_left) / plot_width,
            height = 1.04 * (rect_top - rect_bot) / plot_height,
            r = unit(20, 'points'),
            gp = gpar(
              fill = '#bdbdbd5E',
              col = NA))

        }

        # Add the text
        for (i in 1:length(callout_text_lines)) {
          y_i <- y - (i-1)*strheight(callout_text_lines[i])*font_y_multiplier
          text(x, y_i, labels = callout_text_lines[i],
               cex = callout_text_cfg_n$cex,
               pos = callout_text_cfg_n$pos,
               col = 'grey40')
        }
      }
    }

  } else {
    rm(this_date_callouts, n_callouts)
    plot_fun <- function() { NULL }
  }
  return(plot_fun)
}
