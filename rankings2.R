library(rvest)
library(translateR)

webpage <- read_html("https://thepowerrank.com/world-football-soccer/")

main_html <- html_nodes(webpage, '.content:nth-child(1)')
text <- html_text(main_html)

split_text <- str_split(text, pattern = '\n')
split_text <- split_text[[1]]

counter <- 1
victory_margins <- data.frame(matrix(nrow = 8, ncol = 2))
colnames(victory_margins) <- c("Team", "VM")
for (team in teams) {
  team_temp <- team
  team_index <- which(split_text %in% team_temp)
  team_victory_margin <- split_text[team_index+1]
  
  victory_margins$Team[counter] <- team_temp
  victory_margins$VM[counter] <- as.numeric(team_victory_margin)
  
  counter <- counter + 1
}
