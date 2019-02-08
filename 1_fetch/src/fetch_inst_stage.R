#' @title Download instantaneous stage if no daily stage available.
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_data_ind indicator file for the data.frame of dv_data
#' @param stage_data_ind indicator file for the data.frame of sites with flood stage
#' @param request_limit number indicating how many sites to include per dataRetrieval request (from viz_config.yml)
fetch_inst_stage <- function(ind_file, dv_data_ind, stage_data_ind, request_limit){

  dv_data <- readRDS(scipiper::sc_retrieve(dv_data_ind, remake_file = '1_fetch.yml'))
  sites_stage <- readRDS(scipiper::sc_retrieve(stage_data_ind, remake_file = '1_fetch.yml'))

  dv_data <- readRDS("1_fetch/out/dv_data.rds")
  sites_stage <- readRDS("1_fetch/out/sites_stage.rds")

  sites_calc_stage <- dv_data %>%
    left_join(sites_stage, by = "site_no") %>%
    filter(is.na(GH) & !is.na(flood_stage) & !is.na(Flow))

  # Save posix for single day
  saved_posix <- sites_calc_stage %>%
    select(site_no, dateTime) %>%
    mutate(day = as.Date(dateTime))

  sites_to_calc_stage <- unique(sites_calc_stage$site_no)
  req_bks <- seq(1, length(sites_to_calc_stage), by=request_limit)
  site_sets <- lapply(req_bks, function(r) {
    last_site <- min(r+request_limit-1, length(sites_to_calc_stage))
    get_sites <- sites_to_calc_stage[r:last_site]
    return(get_sites)
  })

  # Go out and fetch instantaneous, then calculate daily averages
  GH_calc <- lapply(site_sets, FUN = function(sites) {

    data_i <-
      dataRetrieval::readNWISdata(
        service = "iv",
        site = sites,
        parameterCd = c("00065"),
        startDate = min(sites_calc_stage[["dateTime"]]),
        endDate = max(sites_calc_stage[["dateTime"]]))

    if(nrow(data_i) > 0) {

      data_i <- data_i %>%
        dataRetrieval::renameNWISColumns() %>%
        mutate(day = as.Date(dateTime))

      if("GH_Inst" %in% names(data_i)) {
        gh_mean <- data_i %>%
          group_by(site_no, day) %>%
          summarize(GH_mean = mean(GH_Inst, na.rm = TRUE))
      } else {
        gh_mean <- gh_mean <- data_i %>%
          group_by(site_no, day) %>%
          summarize(GH_mean = NA)
      }

    } else {
      gh_mean <- NULL
    }

    message(paste("Completed", max(which(unlist(site_sets) %in% sites)),
                  "out of", length(unlist(site_sets))))

    return(gh_mean)
  })

  fixed_gh_data <- bind_rows(GH_calc) %>%
    left_join(saved_posix) %>%
    select(site_no, dateTime, GH_mean, -day)

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(fixed_gh_data, data_file)
  scipiper::gd_put(ind_file, data_file)
}
