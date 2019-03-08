
prep_footnote_fun <- function(footnote_cfg){

  plot_fun <- function(){
    # coordinate space (edges, width, height)
    coord_space <- par()$usr

    title_x <- coord_space[1] + footnote_cfg$x_pos * diff(coord_space[1:2])
    title_y <- coord_space[3] + footnote_cfg$y_pos * diff(coord_space[3:4])

    text(x = title_x, y = title_y,
         labels = paste(footnote_cfg$main, collapse = "\n"),
         cex = footnote_cfg$cex, pos = 4, col = footnote_cfg$col)

  }
  return(plot_fun)
}
