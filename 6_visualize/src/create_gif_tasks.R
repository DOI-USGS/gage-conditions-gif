# create task tables for gif generation

create_timestep_gif_tasks <- function(timestep_ind, storm_track_cfg, folders){

  # prepare a data.frame with one row per task
  timesteps <- readRDS(sc_retrieve(timestep_ind, '2_process.yml'))
  tasks <- data_frame(timestep=timesteps) %>%
    mutate(task_name = strftime(timestep, format = '%Y%m%d_%H', tz = 'UTC'))

  sites_frame <- scipiper::create_task_step(
    step_name = 'sites_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('gage_sites_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_gage_sites_fun(",
        "stage_data = stage_data,",
        "gage_col_config = gage_color_config,",
        "DateTime = I('%s'))"=format(cur_task$timestep, "%Y-%m-%d %H:%M:%S")
      )
    }
  )

  precip_frame <- scipiper::create_task_step( # not sure why this is called "frame".
    step_name = 'precip_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('precip_raster_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("prep_precip_fun(precip_rasters, precip_bins, I('%s'))", format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"))
    }
  )

  spark_frame <- scipiper::create_task_step(
    step_name = 'spark_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('spark_line_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_spark_line_fun(",
        "stage_data,",
        "site_data,",
        "timestep_ind = '2_process/out/timesteps.rds.ind',",
        "sparkline_placement,",
        "gage_color_config,",
        "I('%s'),"=cur_task$timestep,
        "legend_text_cfg = legend_text_cfg)")
    }
  )

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

  legend_frame <- scipiper::create_task_step(
    step_name = 'legend_frame',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf('legend_fun_%s', task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "prep_legend_fun(",
        "precip_bins = precip_bins,",
        "legend_styles = legend_styles,",
        "timesteps_ind = '2_process/out/timesteps.rds.ind',",
        "storm_points_sf = storm_points_sf,",
        "DateTime = I('%s')," = format(cur_task$timestep, "%Y-%m-%d %H:%M:%S"),
        "x_pos = legend_x_pos,",
        "y_pos = legend_y_pos,",
        "legend_text_cfg = legend_text_cfg)"
      )
    }
  )

  gif_frame <- scipiper::create_task_step(
    step_name = 'gif_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_animation_frame(",
        "png_file=target_name,",
        "config=storm_frame_config,",
        "view_fun,",
        "basemap_fun,",
        "ocean_name_fun,",
        "precip_raster_fun_%s,"=cur_task$tn,
        "spark_line_%s,"= cur_task$tn,
        "rivers_fun,",
        "gage_sites_fun_%s,"=cur_task$tn,
        "legend_fun_%s,"=cur_task$tn,
        "datetime_fun_%s,"=cur_task$tn,
        "cities_fun,",
        "watermark_fun)",
        #"streamdata_%s,"= cur_task$tn,
        sep="\n      ")
    }
  )
  gif_test_frame <- scipiper::create_task_step(
    step_name = 'gif_test_frame',
    target_name = function(task_name, step_name, ...){
      file.path(folders$tmp, sprintf('gif_TEST_frame_%s.png', task_name))
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf(
        "create_animation_frame(",
        "png_file=target_name,",
        "config=storm_frame_config,",
        "view_fun,",
        "basemap_fun,",
        "ocean_name_fun,",
        "legend_fun_%s,"=cur_task$tn,
        "bbox_fun,",
        "datetime_fun_%s,"=cur_task$tn,
        "cities_fun,",
        "watermark_fun)",
        sep="\n      ")
    }
  )

  step_list <- list(
    sites_frame, precip_frame,
    spark_frame, datetime_frame, legend_frame, gif_frame, gif_test_frame)
  step_list <- step_list[!sapply(step_list, is.null)]
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=step_list,
    add_complete=FALSE,
    final_steps='gif_frame',
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
