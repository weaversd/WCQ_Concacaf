library(rvest)
library(translateR)

webpage <- read_html("http://espndeportes.espn.com/futbol/spi/rankings/_/view/region/group/concacaf")
team_names_html <- html_nodes(webpage, '.main-links a')
teams_esp <- html_text(team_names_html)

offense_html <- html_nodes(webpage, 'p:nth-child(2) span')
offense <- html_text(offense_html)

defense_html <- html_nodes(webpage, 'p:nth-child(3) span')
defense <- html_text(defense_html)

SPI_raw <- data.frame(teams_esp, offense, defense)

for (i in 1:nrow(SPI_raw)){
  SPI_raw$TEAM[i] <- iconv(SPI_raw$teams_esp[i],from="UTF-8", to="ASCII//TRANSLIT")
  if (SPI_raw$TEAM[i] == "Estados Unidos") {
    SPI_raw$TEAM[i] <- "United States"
  }
}

SPI_teams <- SPI_raw[SPI_raw$TEAM %in% teams,]
SPI_teams <- SPI_teams[,c(4,2,3)]
