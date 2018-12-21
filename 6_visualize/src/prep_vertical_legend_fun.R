
prep_vertical_legend_fun <- function(x_pos, y_pos, legend_cfg, gage_style,
                                     display_percentiles){

  display_percentiles_num <- lapply(display_percentiles, function(x) as.numeric(x)/100)
  display_percentiles_num$norm <- 0.50 # need to add so that it shows up on the legend

  legend_style <- data.frame(per = unlist(display_percentiles_num, use.names = FALSE)) %>%
    add_style_columns(gage_style, display_percentiles_num) %>%
    mutate(
      legend_text = case_when(
        per == display_percentiles_num$flood ~ "Wettest",
        per == max(display_percentiles_num$normal_range) ~ "Wet",
        per == display_percentiles_num$norm ~ "Normal",
        per == min(display_percentiles_num$normal_range) ~ "Dry",
        per == display_percentiles_num$drought_low ~ "Dryer",
        per == display_percentiles_num$drought_severe ~ "Dryest",
        TRUE ~ ""
      )) %>%
    arrange(desc(per)) # fill top - bottom

  # Add style for missing data
  legend_style <- legend_style %>% bind_rows(data.frame(
    per = NA,
    color =  gage_style$no_percentile$col,
    shape = gage_style$no_percentile$pch,
    size =  gage_style$no_percentile$cex,
    legend_text = "No data"
  ))

  alpha_hex <- 'CC'

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
    legend_style$point_height <- legend_style$size*strheight("O")*legend_cfg$point_mult

    text_pos <- 2
    x_text <- x_loc - point_width*0.55

    y_start <- y_loc - strheight(legend_style$legend_text[1])
    y_loc_n <- y_start
    for(n in 1:nrow(legend_style)) {
      point_height_n <- ifelse(n==1, 0, legend_style$point_height[n-1])
      point_spacing <- ifelse(is.na(legend_style$per[n]), 0.4, 0.7)
      y_loc_n <- y_loc_n - point_height_n*point_spacing
      points(x_loc, y_loc_n, bg = paste0(legend_style$color[n], alpha_hex),
             col = legend_style$color[n], pch = legend_style$shape[n],
             cex = legend_style$size[n]*legend_cfg$point_mult, lwd = 1)
      text(x_text, y_loc_n,
           labels = legend_style$legend_text[n],
           pos = text_pos, col = legend_cfg$text_col, cex = legend_cfg$text_cex)
    }

  }
  return(plot_fun)
}
