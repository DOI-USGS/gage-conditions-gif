# Script to create excel file to share. First build the GIF but with new data.

library(openxlsx)

viz_config <- yaml::yaml.load_file('viz_config.yml')
start_date <- as.Date(viz_config$vizDates$start)
end_date <- as.Date(viz_config$vizDates$end)
timesteps <- seq(start_date, end_date, by = 'days')

## Create a new workbook & add a worksheet
wb <- createWorkbook()
addWorksheet(wb, "Sheet 1")

# Add columns & dates
x <- data.frame(Image = rep("", length(timesteps)),
                Frame_Date = c(timesteps),
                Callout_1 = rep(NA, length(timesteps)),
                Region_1 = rep(NA, length(timesteps)),
                Callout_2 = rep(NA, length(timesteps)),
                Region_2 = rep(NA, length(timesteps)),
                Callout_3 = rep(NA, length(timesteps)),
                Region_3 = rep(NA, length(timesteps)),
                Callout_4 = rep(NA, length(timesteps)),
                Region_4 = rep(NA, length(timesteps)),
                stringsAsFactors = FALSE)
writeData(wb, 1, x)

## Insert images
for(i in seq_along(timesteps)) {
  ts <- timesteps[i]
  insertImage(wb, 1, sprintf("6_visualize/tmp/frame_%s_00.png", format(ts, "%Y%m%d")),
              startRow = i + 1,  startCol = 1)
}

# Resize cells
setColWidths(wb, 1, cols = 1:10, widths = c(83, 14.6, rep(c(75, 25), 4)))
setRowHeights(wb, 1, rows = 1:(length(timesteps)+1),
              heights = c(20, rep(222, length(timesteps))))

# Edit cell style
headerStyle <- createStyle(fontSize = 14, textDecoration = "bold", halign = "left", valign = "center")
dateColStyle <- createStyle(numFmt = "DATE", halign = "left", valign = "center")
commentStyle <- createStyle(halign = "left", valign = "center", wrapText = TRUE)

addStyle(wb, 1, headerStyle, 1, 1:10)
addStyle(wb, 1, dateColStyle, 2:(length(timesteps)+1), 2)
addStyle(wb, 1, commentStyle, 2:(length(timesteps)+1), 3:10, gridExpand = TRUE)

# Save the file
fn <- sprintf("river_conditions_%s_%s_%s.xlsx",
              format(start_date, "%b"),
              format(end_date, "%b"),
              format(start_date, "%Y"))
saveWorkbook(wb, fn, overwrite = TRUE)
