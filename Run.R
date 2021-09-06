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

current_table_output <- subset(current_table, select = -(`Pos Trend`))
write.table(current_table_output, paste0("current_table_output/Matchweek_", matchdays, "_table.csv"),
            sep = ",", row.names = F)


source("Weekly_Analysis.R")