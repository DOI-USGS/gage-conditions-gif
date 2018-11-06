prep_view_fun <- function(view_polygon, view_cfg){
  plot_fun <- function(){
    par(omi = c(0,0,0,0), mai = c(0,0,0,0), bg = view_cfg$background_col)
    plot(view_polygon, col = NA, border = NA, xaxs = 'i', yaxs = 'i')
  }
  return(plot_fun)
}
