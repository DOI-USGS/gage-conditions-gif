
prep_datewheel_fun <- function(dateTime, viz_config, dates_config, datewheel_cfg, callouts_cfg){

  # info to setup wheel
  start_dt <- as.Date(dates_config[["start"]])
  end_dt <- as.Date(dates_config[["end"]])
  n_days <- as.numeric(end_dt - start_dt) + 1 # add one to count the day you are subtracting from

  wedge_width <- 2*pi/n_days
  start_angle <- pi # horizontal left side is October
  rot_dir <- -1 # for clockwise; use +1 for counter-clockwise
  end_angle <- start_angle + wedge_width*n_days*rot_dir
  col_background <- viz_config[["background_col"]]

  # info to pinpoint current date
  this_date <- as.Date(dateTime)
  this_date_n <- as.numeric(this_date - start_dt) + 1

  # need to create callouts for before and after date.
  wheel_callouts <- lapply(callouts_cfg, function(x) {
    if(!is.null(x$wheel_color)) {
      return(x)
    } else {
      return(NULL)
    }
  })

  # keep only non-NULL elements
  if(length(wheel_callouts)>0) {
    wheel_callouts <- wheel_callouts[!unlist(lapply(wheel_callouts, is.null))]
    event_ends <- as.Date(unlist(lapply(lapply(wheel_callouts, `[[`, "dates"), `[[`, "end")))
    wheel_callouts <- wheel_callouts[order(event_ends)] # order chronologically in case they aren't already
    event_ends <- event_ends[order(event_ends)]
    n_callouts <- length(wheel_callouts)
  } else {
      n_callouts = 0
    }

  make_arc <- function(x0, y0, r, from_angle, to_angle, rot_dir){
    theta <- seq(from_angle, to_angle, by = rot_dir*0.002)
    x_out <- x0 + r*cos(theta)
    y_out <- y0 + r*sin(theta)

    return(list(x = x_out, y = y_out))
  }

  rm(viz_config, dates_config, callouts_cfg)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr

    wheel_radius <- datewheel_cfg$wheel_per*diff(coord_space[1:2])/2 # 20% of the x
    event_radius <- datewheel_cfg$event_per*wheel_radius
    inner_radius <- datewheel_cfg$inner_per*wheel_radius
    text_radius <- datewheel_cfg$text_per*inner_radius
    x_center <- coord_space[1] + datewheel_cfg$x_pos * diff(coord_space[1:2])
    y_center <- coord_space[3] + datewheel_cfg$y_pos * diff(coord_space[3:4])

    # Calculate where to put month labels
    date_df <- data.frame(day = seq.Date(start_dt, end_dt, by="days")) %>%
      mutate(month = format(day, "%b"))
    sum_dates <- date_df %>%
      group_by(month) %>%
      # find first day of each month
      summarize(first_day = min(day)) %>%
      mutate(first_day_n = as.numeric(first_day - start_dt) + 1) %>%
      # calculate the angle at which to put them and then figure out x/y coords
      mutate(angle_n = start_angle + first_day_n*wedge_width*rot_dir) %>%
      mutate(x = x_center + text_radius*cos(angle_n),
             y = y_center + text_radius*sin(angle_n)) %>%
      select(month, x, y)

    # Create the whole wheel
    segments_wheel <- make_arc(x_center, y_center,
                               r = wheel_radius,
                               from_angle = start_angle,
                               to_angle = end_angle,
                               rot_dir = rot_dir)
    polygon(c(x_center, segments_wheel$x, x_center),
            c(y_center, segments_wheel$y, y_center),
            border = NA, col = datewheel_cfg$col_empty)

    if (n_callouts >0) {
    # Call out arcs are on top of light grey wheel, but below dark grey
      for(n in n_callouts:1) {
        # loop in reverse order so that potentially overlapping events
        # events that start after others are drawn first
        this_callout <- wheel_callouts[[n]]

        # Find event dates
        start_date_event <- as.Date(this_callout$dates$start)
        start_date_event_n <- as.numeric(start_date_event - start_dt) + 1
        end_date_event <- as.Date(this_callout$dates$end)
        end_date_event_n <- as.numeric(end_date_event - start_dt) + 1

        # Increase size of event arc if it overlaps a previous arc
        event_radius_i <- event_radius
        i <- which(end_date_event == event_ends)
        if(i != 1) {
          # if i==1, then this is the first event, so it won't overlap anything
          if(any(start_date_event < event_ends[1:i-1])){
            # if this event starts before any others finish, need to make it 10% bigger
            # this method currently only works for two overlapping events
            # and would need to change if there are more
            event_radius_i <- event_radius + event_radius*0.10
          }
        }

        # Determine where on the wheel the event exists
        start_angle_event <- start_angle + start_date_event_n*wedge_width*rot_dir
        end_angle_event <- start_angle + end_date_event_n*wedge_width*rot_dir

        # Create the event wheel
        callouts_wheel <- make_arc(x_center, y_center,
                                   r = event_radius_i,
                                   from_angle = start_angle_event,
                                   to_angle = end_angle_event,
                                   rot_dir = rot_dir)
        polygon(c(x_center, callouts_wheel$x, x_center),
                c(y_center, callouts_wheel$y, y_center),
                border = datewheel_cfg$col_empty, lwd = 2,
                col = this_callout$wheel_color)

      }
    }

    # Increment the wheel for the date
    end_angle_n <- start_angle + this_date_n*wedge_width*rot_dir
    segments_n <- make_arc(x_center, y_center,
                           r = wheel_radius,
                           from_angle = start_angle,
                           to_angle = end_angle_n,
                           rot_dir = rot_dir)
    polygon(c(x_center, segments_n$x, x_center),
            c(y_center, segments_n$y, y_center),
            border = NA, col = datewheel_cfg$col_filled)

    # Make it a donut
    segments_middle <- make_arc(x_center, y_center,
                                r = inner_radius,
                                from_angle = start_angle,
                                to_angle = end_angle,
                                rot_dir = rot_dir)
    polygon(c(x_center, segments_middle$x, x_center),
            c(y_center, segments_middle$y, y_center),
            border = NA, col = col_background)

    # Add month labels
    text(sum_dates$x, sum_dates$y,
         labels = sum_dates$month,
         col = datewheel_cfg$col_text_months,
         cex = datewheel_cfg$cex_text_months)

    # Add exact day in the center
    text(x_center, y_center,
         col = datewheel_cfg$col_text_datestamp,
         cex = datewheel_cfg$cex_text_datestamp,
         label = format(this_date, "%b %d"), pos = 3)
    text(x_center, y_center,
         col = datewheel_cfg$col_text_datestamp,
         cex = datewheel_cfg$cex_text_datestamp,
         label = format(this_date, "%Y"), pos = 1)
  }
  return(plot_fun)
}
