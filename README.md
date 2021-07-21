# WCQ_Concacaf
A repo to keep track of and predict world cup qualification for concacaf teams

Enter the scores for each team in 'scores.xlsx' and then ensure that the correct 'xlsx' file is referenced in 'workspace_setup.R'

Run 'Run.R' for a summary that contains the current table with relavent statistics as well as %probabilities for a top 3 or top 4 finish for each team based on current results and predicted future performance. Also produces a 'magic number' plot (see below).

Working directory must contain all files along with the scores file.


Alternatively, for more control: open and run files in this order:  
* workspace_setup.R
* functions.R
* rankings2.R
* predict_game2.R
* Simulate_season.R
* simulate_many_seasons.R
* Final Summary.R  

The other files are old versions.



Predictions are based on margin of victory against an average team from [The Power Rank](https://thepowerrank.com/world-football-soccer/), and using a method based on [fivethirtyeight](https://fivethirtyeight.com/methodology/how-our-club-soccer-predictions-work/)'s soccer predictions.  

'Magic number' plot is based on [Playoff Magic](https://www.playoffmagic.com/fifa/major-league-soccer/)'s format. In this case, the first number is the number of points required to secure a spot, and the second number is the total number of points remaining for a team. Remember that the points (for the first number) can come from the team earning them, or the team above/below dropping them.
