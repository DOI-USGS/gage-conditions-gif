#' @title Download instantaneous stage if no daily stage available.
#'
#' @param ind_file character file name where the output should be saved
#' @param dv_data_ind indicator file for the data.frame of dv_data
#' @param stage_data_ind indicator file for the data.frame of sites with flood stage
fetch_inst_stage <- function(ind_file, dv_data_ind, stage_data_ind){

  dv_data <- readRDS(scipiper::sc_retrieve(dv_data_ind, remake_file = '1_fetch.yml'))
  sites_stage <- readRDS(scipiper::sc_retrieve(stage_data_ind, remake_file = '1_fetch.yml'))

  sites_calc_stage <- dv_data %>%
    left_join(sites_stage, by = "site_no") %>%
    filter(is.na(GH) & !is.na(flood_stage) & !is.na(Flow))

  # Go out and fetch instantaneous, then calculate daily averages
  sites_calc_stage$GH_calc <- apply(sites_calc_stage, 1, FUN = function(d) {
    data_i <-
      dataRetrieval::readNWISdata(
        service = "iv",
        site = d[["site_no"]],
        parameterCd = c("00065"),
        startDate = d[["dateTime"]],
        endDate = d[["dateTime"]]) %>%
      dataRetrieval::renameNWISColumns()

    gh_mean <- ifelse("GH_Inst" %in% names(data_i),
                      yes = mean(data_i$GH_Inst),
                      no = NA)

    return(gh_mean)
  })

  fixed_gh_data <- sites_calc_stage %>%
    select(site_no, dateTime, GH_calc)

  # Write the data file and the indicator file
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(sites_calc_stage, data_file)
  scipiper::gd_put(ind_file, data_file)
}
