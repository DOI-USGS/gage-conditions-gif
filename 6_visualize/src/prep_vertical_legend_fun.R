
prep_vertical_legend_fun <- function(percentiles_str, sites_color_palette,
                            x_pos, y_pos, legend_cfg){

  col_fun <- colorRamp(sites_color_palette$with_percentile)
  percentiles <- as.numeric(percentiles_str)/100
  legend_cols <- sapply(percentiles, FUN = function(x){
    rgb(col_fun(x), maxColorValue = 255)
  })

  alpha_hex <- 'CC'

  rm(percentiles)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr
    coord_width <- diff(coord_space[1:2])
    coord_height <- diff(coord_space[3:4])

    x_loc <- coord_space[1] + coord_width * legend_cfg$x_pos
    y_loc <- coord_space[3] + coord_height * legend_cfg$y_pos

    # percentage of X domain for circle diameter; assumes 175 is max cex
    # used for spacing circles along the x and y directions
    point_width <- legend_cfg$point_cex*strwidth("O")
    point_height <- legend_cfg$point_cex*strheight("O")

    # Legend text
    bottom_text <- "Lower"
    mid_text <- "Normal"
    top_text <- "Higher"

    y_start <- y_loc - strheight(top_text)
    legend_cols <- rev(legend_cols) # fill top - bottom

    for(n in 1:length(legend_cols)) {
      y_loc_n <- y_start - (n-1)*point_height*0.55
      points(x_loc, y_loc_n, bg = paste0(legend_cols[n], alpha_hex),
             col = legend_cols[n], pch = 21, cex = legend_cfg$point_cex, lwd = 1)
    }

    # correct for shrinking text size from nested atop() calls
    text_size <- legend_cfg$text_cex
    text_size_bot_top <- text_size #+ 0.5

    text_pos <- 2
    x_text <- x_loc - point_width*0.3

    y_text_midpoint <- mean(c(y_start, y_loc_n))
    y_text_mid <- y_text_midpoint - point_height*0.06
    y_text_bot <- y_text_midpoint - (y_text_midpoint - y_loc_n)*0.87
    y_text_top <- y_text_midpoint + (y_start - y_text_midpoint)*0.86

    # Add text to show "Lower" vs "Higher"
    text(x_text, y_text_bot,
         labels = bottom_text,
         pos = text_pos, col = legend_cfg$text_col, cex = text_size_bot_top)
    text(x_text, y_text_mid, mid_text, pos = text_pos,
         col = legend_cfg$text_col, cex = text_size)
    text(x_text, y_text_top,
         labels = top_text,
         pos = text_pos, col = legend_cfg$text_col, cex = text_size_bot_top)

  }
  return(plot_fun)
}
