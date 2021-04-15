load('workspace.RData')

##################################
### Sportradar Data Collection ###
##################################

library(rjson)

### Pulling Games ###

ncaa_key = "9z6b2gcn3snzetkt2eps6gjp"

# Pull from 2019-2020 season
ncaa_sched19 = fromJSON(file=paste0("https://api.sportradar.us/ncaamb/trial/v7/en/games/2019/reg/schedule.json?api_key=",ncaa_key))

# Grab all game IDs
game_ids19 = NULL
for(i in 1:length(ncaa_sched19$games)){
  if(ncaa_sched19$games[[i]]$status=="closed"){
    game_ids19 = c(game_ids19,ncaa_sched19$games[[i]]$id)
  }
}

# Pull all games' play by play data
pbp19 = list()
for(i in 101:500){
  pbp19[[i]] = fromJSON(file=paste0("https://api.sportradar.us/ncaamb/trial/v7/en/games/",game_ids19[i],"/pbp.json?api_key=",ncaa_key))
  Sys.sleep(1)
}

# Identify games where tracking was used
track <- sapply(pbp19, function(x){x$track_on_court==T})

pbp19_track <- pbp19[track]

# Save tracked games
save(pbp19,file='raw_pbp_data.RData')


### Pulling just necessary variables from each game ###
gm_logs <- list()

for (i in 1:length(pbp19_track)){
  # Select one game at a time
  gm <- c(pbp19_track[[i]]$periods[[1]]$events, pbp19_track[[i]]$periods[[2]]$events)
  gm_logs[[i]] <- data.frame(matrix(data=NA,nrow=length(gm),ncol=5))
  colnames(gm_logs[[i]]) <- c('event','desc','home_names','away_names','possession')
  
  # Loop through each event in a game
  for (j in 1:length(gm)){
    # Assign event type and description
    gm_logs[[i]]$event[j] <- gm[[j]]$event_type
    gm_logs[[i]]$desc[j] <- gm[[j]]$description
    
    # Assign home/away players
    h_players <- unlist(gm[[j]]$on_court$home$players)
    a_players <- unlist(gm[[j]]$on_court$away$players)
    
    gm_logs[[i]]$home_names[j] <- list(unname(h_players[names(h_players)=='full_name']))
    gm_logs[[i]]$away_names[j] <- list(unname(a_players[names(a_players)=='full_name']))
    
    # Assign possession
    if(!is.null(gm[[j]]$attribution$market)){
      market <- gm[[j]]$attribution$market
    } else {
      market <- NA
    }
    
    
    if (!is.na(market)) {
      if (market==pbp19_track[[i]]$home$market) {
        gm_logs[[i]]$possession[j] <- 'home'
        gm_logs[[i]]$market[j] <- market
      } else {
        gm_logs[[i]]$possession[j] <- 'away'
        gm_logs[[i]]$market[j] <- market
      }
    } else {
      gm_logs[[i]]$possession[j] <- NA
      gm_logs[[i]]$market[j] <- NA
    }
    
  }
}

gm_log <- data.table::rbindlist(gm_logs, idcol=T)


### Pulling schedule and team info ###
team_ids19 <- c()
for(i in 1:length(pbp19_track)){
  team_ids19 <- c(team_ids19,pbp19_track[[i]]$home$id,pbp19_track[[i]]$away$id)
}
team_ids19 <- unique(team_ids19)

ncaa_roster19 <- list()
for(i in 1:77){
  ncaa_roster19[[i]] = fromJSON(file=paste0("https://api.sportradar.us/ncaamb/trial/v7/en/teams/",team_ids19[i],"/profile.json?api_key=",ncaa_key))
  Sys.sleep(1)
}

save(gm_log, ncaa_roster19, file='clean_pbp_data.RData')