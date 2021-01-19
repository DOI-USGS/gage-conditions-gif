
prep_vertical_legend_fun <- function(x_pos, y_pos, legend_cfg, gage_style,
                                     display_percentiles){

  display_percentiles_num <- lapply(display_percentiles, function(x) as.numeric(x)/100)
  display_percentiles_num$norm <- 0.50 # need to add so that it shows up on the legend

  # Flood and no data conditions don't have a percentile, so add manually
  additional_conditions <- data.frame(
    per = c(NA, NA),
    dv_stage = c(100, NA),
    flood_stage = c(1, NA),
    stringsAsFactors = FALSE
  )

  legend_style <- data.frame(per = unlist(display_percentiles_num, use.names = FALSE),
                             dv_val = NA, dv_stage = NA, flood_stage = NA) %>%
    bind_rows(additional_conditions) %>%
    add_style_columns(gage_style, display_percentiles_num) %>%
    mutate(
      legend_text = case_when(
        !is.na(dv_stage) & dv_stage > flood_stage ~ "Flooding*",
        per == display_percentiles_num$wet ~ "Wettest",
        per == max(display_percentiles_num$normal_range) ~ "Wet",
        per == display_percentiles_num$norm ~ "Normal",
        per == min(display_percentiles_num$normal_range) ~ "Dry",
        per == display_percentiles_num$drought_low ~ "Drier",
        per == display_percentiles_num$drought_severe ~ "Driest",
        is.na(per) & is.na(dv_val) ~ "No data",
        TRUE ~ ""
      )) %>%
    arrange(dv_stage, desc(per)) # fill top - bottom

  # Adjust size of legend to actually be the same size as the other dots
  new_size <- legend_style[which(legend_style$legend_text == "Wettest"), "cex"]
  legend_style[which(legend_style$legend_text == "Flooding*"), "cex"] <- new_size

  rm(display_percentiles_num, gage_style)

  plot_fun <- function(){

    # compute position info shared across multiple legend elements
    coord_space <- par()$usr
    coord_width <- diff(coord_space[1:2])
    coord_height <- diff(coord_space[3:4])

    x_loc <- coord_space[1] + coord_width * legend_cfg$x_pos
    y_loc <- coord_space[3] + coord_height * legend_cfg$y_pos

    # percentage of X domain for circle diameter; assumes 175 is max cex
    # used for spacing circles along the x and y directions
    point_width <- legend_cfg$point_mult*strwidth("O")
    point_height <- legend_cfg$point_mult*strheight("O")
    # legend_style$point_height <- legend_style$cex*strheight("O")*legend_cfg$point_mult
    legend_style$point_height <- median(legend_style$cex)*strheight("O")*legend_cfg$point_mult

    text_pos <- 2
    x_text <- x_loc - point_width*0.75

    y_start <- y_loc - strheight(legend_style$legend_text[1])
    y_loc_n <- y_start
    for(n in 1:nrow(legend_style)) {
      point_height_n <- ifelse(n==1, 0, legend_style$point_height[n-1])
      point_spacing <- ifelse(is.na(legend_style$per[n]), 0.7, 0.7)
      y_loc_n <- y_loc_n - point_height_n*point_spacing
      scale_cex_factor <- ifelse(legend_style$legend_text[n] %in% c("Dry", "Drier", "Driest"),
                                 yes = 1.5,
                                 no = legend_cfg$point_mult)

      points(x_loc, y_loc_n,
             bg = legend_style$bg[n],
             col = legend_style$col[n],
             pch = legend_style$pch[n],
             cex = legend_style$cex[n]*scale_cex_factor,
             lwd = legend_style$lwd[n])
      text(x_text, y_loc_n,
           labels = legend_style$legend_text[n],
           pos = text_pos, col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    }

  }
  return(plot_fun)
}
