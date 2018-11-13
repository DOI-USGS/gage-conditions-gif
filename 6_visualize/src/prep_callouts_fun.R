
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

        for(n in 1:n_callouts) {
          addBox <- ifelse(!is.null(this_date_callouts[[n]]$addBox),
                           this_date_callouts[[n]]$addBox, FALSE)
          callout_text_cfg_n <- this_date_callouts[[n]]$text
          x <- coord_space[1] + callout_text_cfg_n$x_loc * diff(coord_space[1:2])
          y <- coord_space[3] + callout_text_cfg_n$y_loc * diff(coord_space[3:4])
          callouts <- callout_text_cfg_n$label

          font_x_multiplier <- 2.1 # for Abel
          font_y_multiplier <- 3 # for Abel

          for (i in 1:length(callouts)) {
            y_i <- y - (i-1)*strheight(callouts[i])*font_y_multiplier
            text(x, y_i, labels = callouts[i],
                 cex = callout_text_cfg_n$cex,
                 pos = callout_text_cfg_n$pos,
                 col = 'grey40')
          }

          if(addBox) {
            max_strwidth <- max(strwidth(callouts))*font_x_multiplier # buffer for abel since it can't do that correctly
            max_strheight <- max(strheight(callouts))*font_y_multiplier

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
              0 # default
            )
            y_buffer_bottom <- switch(
              as.character(callout_text_cfg_n$pos),
              "NULL" = 0,
              "1" = 0, # below is the default position for text
              "3" = -max_strheight, # above
              0 # default
            )

            rect(xleft = x - x_buffer_left,
                 xright = x + x_buffer_right,
                 ybottom = y_i - max_strheight - y_buffer_bottom,
                 ytop = y + y_buffer_top,
                 col = "#bdbdbd5E", border = NA)
          }
        }
      }

  } else {
    rm(this_date_callouts, n_callouts)
    plot_fun <- function() { NULL }
  }
  return(plot_fun)
}
