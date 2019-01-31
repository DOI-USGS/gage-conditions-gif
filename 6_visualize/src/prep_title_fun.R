
prep_title_fun <- function(title_cfg){

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr
    font_y_multiplier <- 3.4 # for Abel

    title_x <- coord_space[1] + title_cfg$x_pos * diff(coord_space[1:2])
    title_y <- coord_space[3] + title_cfg$y_pos * diff(coord_space[3:4])

    text(x = title_x, y = title_y, labels = title_cfg$main,
         cex = title_cfg$main_cex, pos = 4, col = title_cfg$main_col)

    if(!is.null(title_cfg$subtitle)) {
      title_y_sub <- title_y - strheight(title_cfg$subtitle)*font_y_multiplier
      text(x = title_x, y = title_y_sub, labels = title_cfg$subtitle,
           cex = title_cfg$sub_cex, pos = 4, col = title_cfg$sub_col)
    }

    if(!is.null(title_cfg$footnote)) {
      title_y_foot <- title_y_sub - strheight(title_cfg$subtitle)*2.5
      text(x = title_x, y = title_y_foot,
           labels = paste(title_cfg$footnote, collapse=""),
           cex = title_cfg$foot_cex, pos = 4, col = title_cfg$foot_col)
    }

  }
  return(plot_fun)
}
