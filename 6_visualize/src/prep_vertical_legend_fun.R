
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
    bottom_text <- "Lower flows"
    mid_text <- "Normal"
    top_text <- "Higher flows"

    y_start <- y_loc - strheight(top_text)
    legend_cols <- rev(legend_cols) # fill top - bottom

    for(n in 1:length(legend_cols)) {
      y_loc_n <- y_start - (n-1)*point_height*0.55
      points(x_loc, y_loc_n, bg = paste0(legend_cols[n], alpha_hex),
             col = legend_cols[n], pch = 21, cex = legend_cfg$point_cex, lwd = 1)
    }

    text_pos <- 2
    x_text <- x_loc - point_width*0.3

    y_text_shift <- point_height*0.06
    y_text_bot <- y_loc_n - y_text_shift
    y_text_mid <- mean(c(y_start, y_loc_n)) - y_text_shift
    y_text_top <- y_start - y_text_shift

    # Add text to show "Lower" vs "Higher"
    text(x_text, y_text_bot, 'Lower flows', pos = text_pos,
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    text(x_text, y_text_mid, 'Normal', pos = text_pos,
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    text(x_text, y_text_top, 'Higher flows', pos = text_pos,
         col = legend_cfg$text_col, cex = legend_cfg$text_cex)

  }
  return(plot_fun)
}