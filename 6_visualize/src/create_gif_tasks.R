# create task tables for gif generation

create_timestep_gif_tasks <- function(timestep_ind, folders){

  # prepare a data.frame with one row per task
  timesteps <- readRDS(sc_retrieve(timestep_ind, '2_process.yml'))
  tasks <- data_frame(timestep=timesteps) %>%
    mutate(task_name = strftime(timestep, format = '%Y%m%d_%H', tz = 'UTC'))

  # ---- timestep-specific png plotting layers ---- #

  datetime_frame <- scipiper::create_task_step(
    step_name = 'datetime_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('datetime_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_datetime_fun(I('%s'), datetime_placement, date_display_tz)", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  gage_sites <- scipiper::create_task_step(
    step_name = 'gage_sites',
    target_name = function(task_name, step_name, ...){
      sprintf('gage_sites_plot_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_gage_sites_fun(percentile_color_data_ind = '2_process/out/dv_stat_colors.rds.ind',
              sites_sp = site_locations_shifted, gage_style_config, dateTime=I('%s'))", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    },
    depends = "2_process/out/dv_stat_colors.rds"
  )

  # ---- main target for each task: the

  complete_png <- scipiper::create_task_step(
    step_name = 'complete_png',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_animation_frame(",
        "png_file=target_name,",
        "config=timestep_frame_config,",
        "view_fun,",
        "watermark_fun,",
        "gage_sites_plot_fun_%s,"=cur_task$tn,
        "datetime_fun_%s)"=cur_task$tn
      )
    }
  )

  # ---- combine into a task plan ---- #

  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      datetime_frame,
      gage_sites,
      complete_png),
    add_complete=FALSE,
    final_steps='complete_png',
    ind_dir=folders$log)
}

# helper function to sprintf a bunch of key-value (string-variableVector) pairs,
# then paste them together with a good separator for constructing remake recipes
psprintf <- function(..., sep='\n      ') {
  args <- list(...)
  non_null_args <- which(!sapply(args, is.null))
  args <- args[non_null_args]
  argnames <- sapply(seq_along(args), function(i) {
    nm <- names(args[i])
    if(!is.null(nm) && nm!='') return(nm)
    val_nm <- names(args[[i]])
    if(!is.null(val_nm) && val_nm!='') return(val_nm)
    return('')
  })
  names(args) <- argnames
  strs <- mapply(function(template, variables) {
    spargs <- if(template == '') list(variables) else c(list(template), as.list(variables))
    do.call(sprintf, spargs)
  }, template=names(args), variables=args)
  paste(strs, collapse=sep)
}
