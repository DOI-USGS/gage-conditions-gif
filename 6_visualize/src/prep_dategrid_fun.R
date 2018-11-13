
prep_dategrid_fun <- function(dateTime, dates_config, dategrid_cfg){

  # info to setup grid
  start_dt <- as.Date(dates_config[["start"]])
  end_dt <- as.Date(dates_config[["end"]])
  n_days <- as.numeric(end_dt - start_dt) + 1 # add one to count the day you are subtracting from
  n_cols <- 7
  n_rows <- ceiling(n_days / n_cols)
  last_col_in_last_row <- ifelse(n_days%%n_cols == 0, n_cols, n_days%%n_cols)

  # info to pinpoint current date
  this_date <- as.Date(dateTime)
  this_date_n <- as.numeric(this_date - start_dt) + 1
  this_date_row <- ceiling(this_date_n / n_cols)
  this_date_col <- ifelse(this_date_n %% n_cols == 0, yes = 7, no = this_date_n %% n_cols)

  rm(dates_config)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr

    # determine how big each sqare should be & where the grid should start
    box_width <- box_height <- strwidth("O")
    box_start_x <- coord_space[1] + dategrid_cfg$xleft * diff(coord_space[1:2])
    box_start_y <- coord_space[3] + dategrid_cfg$ytop * diff(coord_space[3:4])

    # loop by row
    for(r in 1:n_rows) {

      # determine where to put this row vertically
      y_draw <- box_start_y - (r-1)*box_height*1.2

      # loop by column
      for(n in 1:n_cols) {
        if(r == n_rows && n > last_col_in_last_row) {
          # if we are on the last row and beyond the last square, skip drawing anything
          # the last row is likely not full (365/7 = 52.14, so we need 53 rows
          #   with only 1 square in the last row)
          next()
        }

        # determine where to place this square horizontally
        x_draw <- box_start_x + (n-1)*box_width*1.2
        box_col <- ifelse(r == this_date_row && n == this_date_col, "blue", "grey")
        rect(xleft = x_draw, xright = x_draw + box_width,
             ytop = y_draw, ybottom = y_draw - box_height,
             col = box_col, border = NA)
      }
    }

  }
  return(plot_fun)
}
