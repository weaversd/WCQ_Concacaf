setwd("~/Personal_projects/WCQ_Concacaf")

source("workspace_setup.R")
source("functions.R")
source("rankings2.R")
source("predict_game2.R")
source("Simulate_season.R")
source("simulate_many_seasons.R")
source("Final Summary.R")

magic_plot
out
print_final()

library("htmltools")
library("webshot")

current_table_output <- subset(current_table, select = -(`Pos Trend`))
write.table(current_table_output, paste0("current_table_output/Matchweek_", matchdays, "_table.csv"),
            sep = ",", row.names = F)


path <- html_print(out, background = "white", viewer = NULL)
url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
webshot(url,
        file = paste0("current_table_output/WCQ_table.png"),
        selector = ".formattable_widget",
        delay = 0.2)


source("Weekly_Analysis.R")

