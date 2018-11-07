
prep_legend_fun <- function(percentiles_str, sites_color_palette,
                            x_pos = c('left', 'right'), y_pos = c('bottom','top'),
                            legend_cfg){

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  col_fun <- colorRamp(sites_color_palette)
  percentiles <- as.numeric(percentiles_str)/100
  legend_cols <- sapply(percentiles, FUN = function(x){
    rgb(col_fun(x), maxColorValue = 255)
  })

  alpha_hex <- 'CC'

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr

    # percentage of X domain for circle diameter; assumes 175 is max cex
    # used for spacing circles along the x and y directions
    point_perc <- legend_cfg$point_cex/175
    point_dia_usr <- point_perc*diff(coord_space[1:2])

    # Legend text
    left_text <- "Lower flows"
    mid_text <- "Normal"
    right_text <- "Higher flows"

    if (x_pos == 'left'){
      x_edge <- coord_space[1] + strwidth(left_text)
      shift_xdir <- 1
    } else if (x_pos == 'right'){
      x_edge <- coord_space[2] - strwidth(right_text)
      shift_xdir <- -1
    }

    if (y_pos == 'bottom'){
      y_edge <- coord_space[3] + point_dia_usr*0.75 + strheight("G")*1.5 # the 1.5 multiplier captures letters below the line (e.g. g,j,p)
      shift_ydir <- 1
    } else if (y_pos == 'top'){
      y_edge <- coord_space[4] - point_dia_usr*0.75 - strheight("Gg")*1.5
      shift_ydir <- -1
    }

    if(legend_cfg$y_text_pos == 'under'){
      shift_ytext <- -1
    } else if (legend_cfg$y_text_pos == 'over'){
      shift_ytext <- 1
    }
    y_text <- y_edge + (point_dia_usr*shift_ytext) + (point_dia_usr*0.1*shift_ytext)

    x_start <- x_edge + point_dia_usr/2*shift_xdir
    for(n in 1:length(legend_cols)) {
      x_loc <- x_start + (n-1)*point_dia_usr*shift_xdir
      points(x_loc, y_edge, bg = paste0(legend_cols[n], alpha_hex),
             col = legend_cols[n], pch = 21, cex = legend_cfg$point_cex, lwd = 1)
    }

    x_left <- ifelse(x_pos == "left", x_start, x_loc)
    x_right <- ifelse(x_pos == "right", x_start, x_loc)

    # Add text to show "Lower" vs "Higher"
    text(x_left + strwidth("flows"), y_text, 'Lower flows', pos = 2,
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    text(mean(c(x_start, x_loc)), y_text, 'Normal',
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    text(x_right - strwidth("flows"), y_text, 'Higher flows', pos = 4,
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)

  }
  return(plot_fun)
}
