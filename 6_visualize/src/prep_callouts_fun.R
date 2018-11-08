
prep_callout_fun <- function(callout_text_cfg){

  plot_fun <- function(){

    # it is up to the user to parse the text to make sure it doesn't end up outside of the margins@
    coord_space <- par()$usr
    x <- callout_text_cfg$x_loc * diff(coord_space[1:2])
    y <- callout_text_cfg$y_loc * diff(coord_space[3:4])
    callouts <- callout_text_cfg$label

    for (i in 1:length(callouts)) {
      y_i <- y + (i-1)*strheight(callouts)
      text(x, y_i, labels = callouts, cex = 1.6, pos = callout_text_cfg$pos, col = 'grey40')
    }
  }
  return(plot_fun)
}
