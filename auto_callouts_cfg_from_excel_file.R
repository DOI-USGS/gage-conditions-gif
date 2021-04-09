# Reading Excel file back in
# Format and then create `callouts_cfg.yml`
# Use that file as "callouts_cfg" in 6_visualize
# Then, manually edit the rest of `callouts_cfg.yml` to get text looking correct

library(dplyr)
fn_in <- "river_conditions_Jan_Mar_2021_reviewers_reconciled.xlsx"
callout_data_raw <- openxlsx::read.xlsx(fn_in, fillMergedCells = TRUE, detectDates = TRUE)

callout_data <- callout_data_raw

callouts_cfg <- callout_data_raw %>%
  # filter(!is.na(Callout_1)) %>%
  select(-Image)

callouts_cfg_fmt <- callouts_cfg %>%
  # Transform to long format
  tidyr::unite("Callout_1Region_1", "Callout_1", "Region_1", sep="_") %>%
  tidyr::unite("Callout_2Region_2", "Callout_2", "Region_2", sep="_") %>%
  tidyr::unite("Callout_3Region_3", "Callout_3", "Region_3", sep="_") %>%
  tidyr::unite("Callout_4Region_4", "Callout_4", "Region_4", sep="_") %>%
  tidyr::unite("Callout_5Region_5", "Callout_5", "Region_5", sep="_") %>%
  tidyr::gather(key = "Num", value = "CalloutRegion", -Frame_Date) %>%
  select(-Num) %>%
  # `convert=TRUE` will put NAs back as NAs rather than the string, "NA"
  tidyr::separate(CalloutRegion, c("Callout", "Region"), sep="_", convert=TRUE) %>%
  # Eliminate any NA callouts
  filter(!is.na(Callout)) %>%
  group_by(Callout) %>%
  summarize(start_date = min(Frame_Date), end_date = max(Frame_Date)) %>%
  select(label = Callout, everything()) %>%
  arrange(start_date)

# Turn data frame into list

callouts_list <- split(callouts_cfg_fmt, seq(nrow(callouts_cfg_fmt)))
callouts_string_list <- lapply(callouts_list, function(x) {
  callout_list_t <- t(x)
  callout_list_data <- setNames(split(callout_list_t, seq(nrow(callout_list_t))), rownames(callout_list_t))
  whisker::whisker.render(readLines("1_fetch/in/callout_template.mustache"), data = callout_list_data)
})

# Save output as file
writeLines(unlist(lapply(callouts_string_list, paste, collapse=" ")), "callouts_cfg.yml")
