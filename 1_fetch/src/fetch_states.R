fetch_states <- function(ind_file) {
  # download shape bounds from census.gov (why not from maps package?)
  # See https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html for details on this file.
  download.file(
    destfile = scipiper::as_data_file(ind_file),
    url = "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_20m.zip",
    method = "libcurl")

  # post to Drive for sharing/promising
  gd_put(ind_file)
}
