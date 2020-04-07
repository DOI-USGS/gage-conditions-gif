
prep_watermark_fun <- function(watermark_file, x_pos = c('left','right'), y_pos = c('top','bottom')){
  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  plot_fun <- function(){
    watermark_frac <- 0.15 # fraction of the width of the figure
    watermark_bump_frac <- 0.01
    coord_space <- par()$usr

    watermark_alpha <- 0.7
    d <- png::readPNG(watermark_file)

    which_image <- d[,,4] != 0 # transparency
    d[which_image] <- watermark_alpha
    logo <- which(d[,,4] != 0 )

    d[,,4][logo] <- 1 # make the logo part opaque
    coord_width <- coord_space[2]-coord_space[1]
    coord_height <- coord_space[4]-coord_space[3]
    watermark_width <- dim(d)[2]
    img_scale <- coord_width*watermark_frac/watermark_width

    if (x_pos == 'left'){
      x1 <- coord_space[1]+coord_width*watermark_bump_frac
    } else if (x_pos == 'right'){
      x1 <- coord_space[2]-coord_width*watermark_bump_frac-ncol(d)*img_scale
    } else {
      stop('x_pos ', x_pos, ' not implemented yet')
    }

    if (y_pos == 'bottom'){
      y1 <- coord_space[3]+coord_height*watermark_bump_frac
    } else if (y_pos == 'top'){
      y1 <- coord_space[4]-coord_height*watermark_bump_frac-nrow(d)*img_scale
    } else {
      stop('y_pos ', y_pos, ' not implemented yet')
    }


    rasterImage(d, x1, y1, x1+ncol(d)*img_scale, y1+nrow(d)*img_scale)

    # Add visid logo
    # polygon <- png::readPNG("6_visualize/in/usgs_av_lower_left_temp_1080.png")
    # rasterImage(polygon, xleft=coord_space[1], ybottom=coord_space[3], xright=coord_space[2], ytop=coord_space[4])
  }
  return(plot_fun)
}

prep_watermark_for_intro_fun <- function(watermark_file, x_pos = c('left','right'), y_pos = c('top','bottom')){
  # Had to scale this one differently in x and y directions for some reason ....

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  plot_fun <- function(){
    watermark_y_frac <- 0.08 # fraction of the height of the figure
    watermark_x_frac <- 0.15 # fraction of the width of the figure
    watermark_bump_frac <- 0.01
    coord_space <- par()$usr

    watermark_alpha <- 0.7
    d <- png::readPNG(watermark_file)

    which_image <- d[,,4] != 0 # transparency
    d[which_image] <- watermark_alpha
    logo <- which(d[,,4] != 0 )

    d[,,4][logo] <- 1 # make the logo part opaque
    coord_width <- coord_space[2]-coord_space[1]
    coord_height <- coord_space[4]-coord_space[3]
    watermark_width <- dim(d)[2]
    watermark_height <- dim(d)[1]
    img_x_scale <- coord_width*watermark_x_frac/watermark_width
    img_y_scale <- coord_height*watermark_y_frac/watermark_height

    if (x_pos == 'left'){
      x1 <- coord_space[1]+coord_width*watermark_bump_frac
    } else if (x_pos == 'right'){
      x1 <- coord_space[2]-coord_width*watermark_bump_frac-ncol(d)*img_x_scale
    } else {
      stop('x_pos ', x_pos, ' not implemented yet')
    }

    if (y_pos == 'bottom'){
      y1 <- coord_space[3]+coord_height*watermark_bump_frac
    } else if (y_pos == 'top'){
      y1 <- coord_space[4]-coord_height*watermark_bump_frac-nrow(d)*img_y_scale
    } else {
      stop('y_pos ', y_pos, ' not implemented yet')
    }


    rasterImage(d, x1, y1, x1+ncol(d)*img_x_scale, y1+nrow(d)*img_y_scale)

  }
  return(plot_fun)
}

prep_watermark_for_intro_fun <- function(watermark_file, x_pos = c('left','right'), y_pos = c('top','bottom')){
  # Had to scale this one differently in x and y directions for some reason ....

  x_pos <- match.arg(x_pos)
  y_pos <- match.arg(y_pos)

  plot_fun <- function(){
    watermark_y_frac <- 0.08 # fraction of the height of the figure
    watermark_x_frac <- 0.15 # fraction of the width of the figure
    watermark_bump_frac <- 0.01
    coord_space <- par()$usr

    watermark_alpha <- 0.7
    d <- png::readPNG(watermark_file)

    which_image <- d[,,4] != 0 # transparency
    d[which_image] <- watermark_alpha
    logo <- which(d[,,4] != 0 )

    d[,,4][logo] <- 1 # make the logo part opaque
    coord_width <- coord_space[2]-coord_space[1]
    coord_height <- coord_space[4]-coord_space[3]
    watermark_width <- dim(d)[2]
    watermark_height <- dim(d)[1]
    img_x_scale <- coord_width*watermark_x_frac/watermark_width
    img_y_scale <- coord_height*watermark_y_frac/watermark_height

    if (x_pos == 'left'){
      x1 <- coord_space[1]+coord_width*watermark_bump_frac
    } else if (x_pos == 'right'){
      x1 <- coord_space[2]-coord_width*watermark_bump_frac-ncol(d)*img_x_scale
    } else {
      stop('x_pos ', x_pos, ' not implemented yet')
    }

    if (y_pos == 'bottom'){
      y1 <- coord_space[3]+coord_height*watermark_bump_frac
    } else if (y_pos == 'top'){
      y1 <- coord_space[4]-coord_height*watermark_bump_frac-nrow(d)*img_y_scale
    } else {
      stop('y_pos ', y_pos, ' not implemented yet')
    }


    rasterImage(d, x1, y1, x1+ncol(d)*img_x_scale, y1+nrow(d)*img_y_scale)
  }
  return(plot_fun)
}
