source('6_visualize/src/create_animation_frame.R')

test_plot_funs <- function(..., png_file=NA, task_file='6_timestep_gif_tasks.yml') {

  # make sure all the plots are up to date by building them
  plot_fun_names <- sapply(substitute(list(...))[-1], deparse)
  plot_funs <- lapply(plot_fun_names, scmake, remake_file=task_file, verbose=FALSE)

  # call create_animation_frame. we'll always want the view_fun as a base layer
  args <- c(
    list(png_file=png_file,
         config=scmake('view_cfg', verbose=FALSE),
         view_fun=scmake('view_fun', verbose=FALSE)),
    plot_funs)
  do.call(create_animation_frame, args)

}
