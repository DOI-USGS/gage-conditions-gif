# Reading Excel file with 3 columns
#  Start, End, Label
# Format and then create `callouts_cfg.yml`
# Use that file as "callouts_cfg" in 6_visualize
# Then, manually edit the rest of `callouts_cfg.yml` to get text looking correct

library(dplyr)
fn_in <- "river_conditions_Apr_Jun_2022_list.xlsx"

callout_data <- readxl::read_excel(fn_in) %>%
  # Make columns match what the mustache template expects
  rename(start_date = Start,
         end_date = End,
         label = Label)

# Turn data frame into list

callouts_list <- split(callout_data, seq(nrow(callout_data)))
callouts_string_list <- lapply(callouts_list, function(x) {
  callout_list_t <- t(x)
  callout_list_data <- setNames(split(callout_list_t, seq(nrow(callout_list_t))), rownames(callout_list_t))
  whisker::whisker.render(readLines("1_fetch/in/callout_template.mustache"), data = callout_list_data)
})

# Save output as file
writeLines(unlist(lapply(callouts_string_list, paste, collapse=" ")), "callouts_cfg.yml")
