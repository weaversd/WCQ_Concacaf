# WCQ_Concacaf
A repo to keep track of and predict world cup qualification for concacaf teams


open and run files in this order:  
* workspace_setup.R
* functions.R
* rankings.R
* predict_game.R
* Simulate_season.R
* simulate_many_seasons.R

Predictions are based on SPI rankings from [ESPN Deportes](http://espndeportes.espn.com/futbol/spi/rankings/_/view/region/group/concacaf), and using a method based on [fivethirtyeight](https://fivethirtyeight.com/methodology/how-our-club-soccer-predictions-work/)'s soccer predictions.  

'Magic number' plot is based on [Playoff Magic](https://www.playoffmagic.com/fifa/major-league-soccer/)'s format. In this case, the first number is the number of points required to secure a spot, and the second number is the total number of points remaining for a team. Remember that the points (for the first number) can come from the team earning them, or the team above dropping them.
