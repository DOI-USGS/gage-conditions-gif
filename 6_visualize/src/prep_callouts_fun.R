
prep_callouts_fun <- function(callouts_cfg, dateTime){

  this_date <- as.POSIXct(dateTime, tz = "UTC") #watch timezones if we ever switch from daily data
  this_date_callouts <- lapply(callouts_cfg, function(x, this_date) {
    start <- as.POSIXct(x$dates$start, tz = "UTC")
    end <- as.POSIXct(x$dates$end, tz = "UTC")
    if(this_date >= start & this_date <= end) {
      callouts_to_plot <- x
    } else {
      callouts_to_plot <- NULL
    }
    return(callouts_to_plot)
  }, this_date)

  # keep only non-NULL elements
  this_date_callouts <- this_date_callouts[!unlist(lapply(this_date_callouts, is.null))]

  plot_fun <- function(){

    # it is up to the user to parse the text to make sure it doesn't end up outside of the margins
    coord_space <- par()$usr

    n_callouts <- length(this_date_callouts)
    if(n_callouts > 0) {
      for(n in 1:n_callouts) {
        callout_text_cfg_n <- this_date_callouts[[n]]$text
        x <- coord_space[1] + callout_text_cfg_n$x_loc * diff(coord_space[1:2])
        y <- coord_space[3] + callout_text_cfg_n$y_loc * diff(coord_space[3:4])
        callouts <- callout_text_cfg_n$label

        for (i in 1:length(callouts)) {
          y_i <- y - (i-1)*strheight(callouts[i])*2
          text(x, y_i, labels = callouts[i],
               cex = callout_text_cfg_n$cex,
               pos = callout_text_cfg_n$pos,
               col = 'grey40')
        }
      }
    }

  }
  return(plot_fun)
}
