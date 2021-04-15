library(tidyverse)
load('clean_pbp_data.RData')


### Functions ###

# Count frequency of defensive event based on player name
tally_def <- function(event_name, name){
  
  sub1 <- gm_log %>% filter(possession == 'away', event==event_name) # Pull opposite team's events for defensive metrics
  id1 <- sapply(sub1$home_names, FUN=function(x){name %in% x})
  
  sub2 <- gm_log %>% filter(possession == 'home', event==event_name)
  id2 <- sapply(sub2$away_names, FUN=function(x){name %in% x})    
  
  return(nrow(sub1[id1,]) + nrow(sub2[id2,]))
}

# Count frequency of offensive event based on player name
tally_off <- function(event_name, name){
  
  sub1 <- gm_log %>% filter(possession == 'away', event==event_name) # Pull opposite team's events for defensive metrics
  id1 <- sapply(sub1$away_names, FUN=function(x){name %in% x})
  
  sub2 <- gm_log %>% filter(possession == 'home', event==event_name)
  id2 <- sapply(sub2$home_names, FUN=function(x){name %in% x})    
  
  return(nrow(sub1[id1,]) + nrow(sub2[id2,]))
}

# Counting defensive possessions using Nylon Calculus equation (FGA + FT trip – ORB + TOV)
poss_count_def <- function(name){
  # Count of possessions at home
  id_h <- sapply(gm_log$home_names, FUN=function(x){name %in% x})
  if(nrow(gm_log[id_h,] %>% filter(possession=='away'))>0){
    count_h <- nrow(gm_log[id_h,] %>% filter(possession=='away') %>% filter(event %in% c('twopointmade','twopointmiss','threepointmade','threepointmiss'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'turnover'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'2 of 2'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'3 of 3'))) -
               sum(sapply(gm_log[id_h,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'offensive rebound')))
  } else {
    count_h <- 0
  }
  
  # Count of possessions at away
  id_a <- sapply(gm_log$away_names, FUN=function(x){name %in% x})
  if(nrow(gm_log[id_a,] %>% filter(possession=='home'))>0){
    count_a <- nrow(gm_log[id_a,] %>% filter(possession=='home') %>% filter(event %in% c('twopointmade','twopointmiss','threepointmade','threepointmiss'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'turnover'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'2 of 2'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'3 of 3'))) -
               sum(sapply(gm_log[id_a,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'offensive rebound')))
  } else {
    count_a <- 0
  }
  
  return(count_h + count_a)
}

# Counting offensive possessions using Nylon Calculus equation (FGA + FT trip – ORB + TOV)
poss_count_off <- function(name){
  # Count of possessions at home
  id_h <- sapply(gm_log$home_names, FUN=function(x){name %in% x})
  if(nrow(gm_log[id_h,] %>% filter(possession=='home'))>0){
    count_h <- nrow(gm_log[id_h,] %>% filter(possession=='home') %>% filter(event %in% c('twopointmade','twopointmiss','threepointmade','threepointmiss'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'turnover'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'2 of 2'))) +
               sum(sapply(gm_log[id_h,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'3 of 3'))) -
               sum(sapply(gm_log[id_h,] %>% filter(possession=='home') %>% select(desc), function(x) str_detect(x,'offensive rebound')))
  } else {
    count_h <- 0
  }
  
  # Count of possessions at away
  id_a <- sapply(gm_log$away_names, FUN=function(x){name %in% x})
  if(nrow(gm_log[id_a,] %>% filter(possession=='away'))>0){
    count_a <- nrow(gm_log[id_a,] %>% filter(possession=='away') %>% filter(event %in% c('twopointmade','twopointmiss','threepointmade','threepointmiss'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'turnover'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'2 of 2'))) +
               sum(sapply(gm_log[id_a,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'3 of 3'))) -
               sum(sapply(gm_log[id_a,] %>% filter(possession=='away') %>% select(desc), function(x) str_detect(x,'offensive rebound')))
  } else {
    count_a <- 0
  }
  
  return(count_h + count_a)
}



### Assembling table of all players and on/off stats ###

# Pulling all players and teams
players <- data.frame('Player'=NA,'Team'=NA)[numeric(0),]
for (i in 1:length(ncaa_roster19)){
  team <- ncaa_roster19[[i]]$market
  for (j in 1:length(ncaa_roster19[[i]]$players)){
    x <- data.frame('Player'=ncaa_roster19[[i]]$players[[j]]$full_name, 'Team'=team)
    players <- rbind(players, x)
  }
}

# Calculating stats for each player
stats <- data.frame('Player'=players$Player, 'Team'=players$Team,
                    'Possessions'=NA,
                    '2P%'=NA, '2PA'=NA, '3P%'=NA, '3PA'=NA,
                    'Turnover%'=NA,'ShootingFoul%'=NA,'PersonalFoul%'=NA,
                    check.names=F)

start_time <- Sys.time()
for(i in 1:nrow(stats)){
  name <- stats$Player[i]
  team <- stats$Team[i]
  poss_def <- poss_count_def(name)
  poss_off <- poss_count_off(name)
  
  # Number of defensive possessions
  stats$Possessions[i] <- poss_def
  
  # Opponent 2P%
  stats$`2P%`[i] <- tally_def('twopointmade',name) / (tally_def('twopointmade',name) + tally_def('twopointmiss',name))
  
  # Opponent 2PA
  stats$`2PA`[i] <- tally_def('twopointmade',name) + tally_def('twopointmiss',name)
  
  # Opponent 3P%
  stats$`3P%`[i] <- tally_def('threepointmade',name) / (tally_def('threepointmade',name) + tally_def('threepointmiss',name))
  
  # Opponent 3PA
  stats$`3PA`[i] <- tally_def('threepointmade',name) + tally_def('threepointmiss',name)
  
  # Opponent Turnover%
  stats$`Turnover%`[i] <-  tally_def('turnover',name) / poss_def
  
  # Opponent Shooting Foul%
  stats$`ShootingFoul%`[i] <- tally_off('shootingfoul',name) / poss_off
  
  # Opponent Personal Foul%
  stats$`PersonalFoul%`[i] <- tally_off('personalfoul',name) / poss_off
}
Sys.time() - start_time
beepr::beep(5)



write_csv(stats, file='defense_table.csv')