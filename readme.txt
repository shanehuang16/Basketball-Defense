Files:

Sportradar Data Collection.R: Code going from pulling data off sportradar to outputting a cleaned dataframe of play-by-play data for the first 56 games with tracking data

Sportradar Metric Creation.R: Code taking play-by-play data and calculating on-court defensive metrics for all players

app.R: Code to run RShiny app

raw_pbp_data.RData: Raw play-by-play data pulled from sportradar for first 500 games (needed to run 'Sportradar Data Collection.R')

clean_pbp_data.RData: Cleaned play-by-play data and NCAA roster information (needed to run 'Sportradar Metric Creation.R')

defense_table.csv: Data frame with calculated defensive stats (needed to run 'app.R')