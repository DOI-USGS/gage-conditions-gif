create_timestep_gif_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    sources=c(
      '6_visualize/src/create_animation_frame.R',
      '6_visualize/src/prep_datewheel_fun.R'),
    file_extensions=c('feather','ind')#,
    #ind_complete=TRUE
    )
}

create_final_gif_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    sources=c('6_visualize/src/create_final_frame.R',
              '6_visualize/src/prep_callouts_fun.R'), # need prep_callouts for perc_to_hexalpha fxn
    file_extensions=c('feather','ind')#,
    #ind_complete=TRUE
    )
}

create_pause_gif_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c('dplyr', 'scipiper'),
    sources=c(),
    file_extensions=c('feather','ind')#,
    #ind_complete=TRUE
    )
}
