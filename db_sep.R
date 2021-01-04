library(tidyverse)
library(pROC)

#These are the supplemental datasets supplied by the NFL
games <- read.csv("games.csv")
players_nfl <- read.csv("players.csv")
plays <- read.csv("plays.csv")
targets <- read.csv("targetedReciever.csv")

#We'll start by just fucking around with week 1, then will expand later

all <- data.frame()

#You shouldn't have to increase the memory limit. the resulting data frame is ~800 MB
for(i in 1:17){
  print(paste0("Start Week ", toString(i)))
  
  wk1 <- read.csv(paste0("week",toString(i),".csv")) %>%
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  
  #These are the plays which throw errors, it's 14 so I am just throwing these away
  if(i == 3)
  {
    wk1 <- wk1 %>%
      filter(!(gameId == 2018092304 & playId == 1687))
  }
  
  if(i == 4){
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018093012 & playId == 302))
  }
  
  if(i == 6)
  {
    wk1 <- wk1 %>%
      filter(!(gameId == 2018101410 & playId == 431))
  }
  
  if(i == 7)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018102101 & playId == 2787))
  }
  
  if(i == 8)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018102809 & playId == 766))
  }
  
  if(i == 9)
  {
    wk1 <- wk1 %>%
      filter(!(gameId == 2018110410 & playId == 3640))
  }
  
  if(i == 10)
  {
    wk1 <- wk1 %>%
      filter(!(gameId == 2018111106 & playId == 2650)) %>% 
      filter(!(gameId == 2018111106 & playId == 2140))
  }
  
  if(i == 13)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018120206 & playId == 2758))
  }
  
  if(i == 14)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018120903 & playId == 4254))
  }
  
  if(i == 15)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018121601 & playId == 425))
  }
  
  if(i == 16)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018122309 & playId == 998))
  }
  
  if(i == 17)
  {
    wk1 <- wk1 %>% 
      filter(!(gameId == 2018123011 & playId == 1155)) %>% 
      filter(!(gameId == 2018123001 & playId == 1155))
  }
  
  #merging plays and tracking data
  df_merged <- inner_join(games %>% filter(week == i),
                          plays,
                          by = c("gameId" = "gameId"))
  
  #merging games data to previously merged frame
  df_merged <- inner_join(df_merged,
                          wk1,
                          by = c("gameId" = "gameId",
                                 "playId" = "playId")) %>% 
    inner_join(targets)
  
  #Most of this is simply taking out the unnecessary data to the model
  
  df_merged <- df_merged %>% 
    #determining side of ball
    mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
      ((team == "home") &
         (possessionTeam == homeTeamAbbr)) |
        
        #if tracked player is away and away has ball
        ((team == "away") &
           (possessionTeam == visitorTeamAbbr)),
      
      
      #if either condition is true, offense
      "offense",
      
      #if neither condition is true, defense
      "defense"),
      
      #defining defensive team
      defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                             visitorTeamAbbr,
                             homeTeamAbbr))
  
  pass_arrival_events <- c("pass_outcome_incomplete",
                           "pass_arrived",
                           "pass_outcome_intercepted",
                           "pass_outcome_caught",
                           "pass_outcome_touchdown")
  
  pass_release_events <- c("pass_forward",
                           "pass_shovel")
  
  df_merged <- df_merged %>% distinct() %>% 
    group_by(gameId, playId) %>% 
    mutate(arrival_frame = max(frameId[event %in% pass_arrival_events]), snap_frame = max(frameId[event == 'ball_snap'])) %>% #don't need pre-snap data
    filter(frameId <= arrival_frame) %>% 
    filter(frameId > snap_frame) %>% 
    ungroup()
  
  df_merged <- df_merged %>%
    mutate(team = ifelse(team == "home", homeTeamAbbr, visitorTeamAbbr)) %>% 
    group_by(gameId, playId) %>%
    mutate(spike = sum(event == "qb_spike") > 0,
           off_players = sum(team == possessionTeam & displayName != "Football"),
           def_players = sum(team != possessionTeam & displayName != "Football"),
           sack = sum(event == "qb_sack" | event == 'qb_strip_sack') > 0) %>%  
    filter(!spike) %>% 
    select(-spike) %>% 
    filter(!sack) %>% 
    select(-sack) %>% 
    ungroup() %>% 
    filter(off_players > 0 & def_players > 0) %>% 
    select(
      -gameDate,
      -gameTimeEastern,
      -homeTeamAbbr,
      -visitorTeamAbbr,
      -week,
      -quarter,
      -down,
      -yardsToGo,
      -possessionTeam,
      -playType,
      -yardlineNumber,
      -offenseFormation,
      -defendersInTheBox,
      -numberOfPassRushers,
      -personnelD,
      -typeDropback,
      -preSnapVisitorScore,
      -preSnapHomeScore,
      -gameClock,
      -absoluteYardlineNumber,
      -penaltyCodes,
      -penaltyJerseyNumbers,
      -offensePlayResult,
      -playResult,
      -epa,
      -isDefensivePI,
      -time,
      -dis,
      -playDirection,
      -route,
      -o
    )
  
  df_merged <- closest_wr(df_merged)
  
  player_ids <- df_merged %>% select(displayName, nflId) %>% distinct()
  
  df_merged <- df_merged %>% 
    left_join(player_ids, all.x = TRUE) %>% 
    mutate(
      completed = ifelse(nflId != targetNflId, 0, ifelse(passResult == 'C', 1, 0)),
      targeted = ifelse(frameId != arrival_frame, 0, ifelse(nflId == targetNflId, 1, 0))
      )
  
  #comment out this line if you want to animate a given play
  df_merged <- df_merged %>% filter(sideOfBall == 'offense' & position != 'QB')
  
  df_merged <- df_merged %>% 
    mutate(
      s_diff = closest_s - s
    )
  
  df_merged <- df_merged %>% 
    select(
      -yardlineSide,
      -personnelO,
      -x,
      -y,
      -event,
      -nflId,
      -team,
      -off_players,
      -def_players,
      -playDescription,
      -jerseyNumber,
      -dir,
      -targetNflId,
      -snap_frame,
      -s,
      -closest_s,
      -a
    )
  
  all <- bind_rows(all, df_merged)
  
  remove(df_merged)
  
  print(paste0("Done Week ", toString(i)))
}

#This is the main function that find the closest defender to each wr and the geometric factors around that
closest_wr <- function(frame){
  fp_wr <- frame %>% 
    filter(sideOfBall == 'offense') %>% 
    filter(position != 'QB') %>% 
    filter(displayName != 'Football') %>% 
    select(
      gameId, 
      playId, 
      frameId, 
      wr = displayName,
      wr_x = x,
      wr_y = y,
      wr_s = s
    ) %>% 
    mutate(
      wr_next_x = lead(wr_x, order_by = (wr)),
      wr_next_y = lead(wr_y, order_by = wr),
      wr_s_dir = ifelse(wr_x == wr_next_x, ifelse(wr_next_y > wr_y, pi/2, 3*pi/2), atan((wr_next_y - wr_y)/(wr_next_x - wr_x)))
    )
  
  fp_db <- frame %>% 
    filter(sideOfBall == 'defense') %>% 
    filter(displayName != 'Football') %>% 
    select(
      gameId, 
      playId, 
      frameId,
      db = displayName,
      db_x = x,
      db_y = y,
      db_s = s
    ) %>% 
    mutate(
      db_next_x = lead(db_x, order_by = (db)),
      db_next_y = lead(db_y, order_by = db),
      db_s_dir = ifelse(db_x == db_next_x, ifelse(db_next_y > db_y, pi/2, 3*pi/2), atan((db_next_y - db_y)/(db_next_x - db_x)))
    )
  
  fp_fb <- frame %>% 
    filter(displayName == 'Football') %>%
    group_by(gameId, playId, frameId) %>% 
    select(
      fb_x = x,
      fb_y = y
    )
  
  tot <- merge(fp_db, fp_wr, by = c('gameId','frameId','playId'))%>% 
    mutate(
      pythag_dist = sqrt((db_x - wr_x)^2 + (db_y - wr_y)^2)
    ) %>% 
    group_by(gameId, playId, frameId, wr) %>% 
    summarise(
      closest_dist = min(pythag_dist),
      closest = db[pythag_dist == closest_dist],
      closest_x = db_x[db == closest],
      closest_y = db_y[db == closest],
      closest_s = db_s[db == closest],
      closest_s_dir = db_s_dir[db == closest],
      wr_s_dir = wr_s_dir,
      wr_x = wr_x,
      wr_y = wr_y
    ) %>% 
    mutate(
      sideline_dist = min(ifelse(wr_x < 60, wr_x, 120 - wr_x), ifelse(wr_y < 53.333/2, wr_y, 53.333 - wr_y)),
      proj_s_per = ifelse(closest_s == 0, 0, (closest_s*(cos(wr_s_dir - closest_s_dir)))/closest_s)
    ) %>% 
    rename("displayName" = "wr") %>%
    distinct() 
  
  # view(tot %>% head(25))
  
  tot <- tot %>% inner_join(fp_fb) %>% 
    group_by(gameId, playId) %>%
    mutate(
      wr_fb_dist = sqrt((wr_x - fb_x)^2 + (wr_y - fb_y)^2),
      wr_db_dist = sqrt((wr_x - closest_x)^2 + (wr_y - closest_y)^2),
      db_fb = sqrt((closest_x - fb_x)^2 + (closest_y - fb_y)^2),
      theta_fb_db_wr = acos((((closest_x - fb_x) * (closest_x - wr_x)) + ((closest_y - fb_y) * (closest_y - wr_y)))/(db_fb * wr_db_dist)),
      theta_fb_db_wr = (180/pi)*ifelse(theta_fb_db_wr > pi, theta_fb_db_wr - pi, theta_fb_db_wr)
    ) %>%
    ungroup() %>%
   mutate(
    dist = wr_fb_dist,
    theta = theta_fb_db_wr,
  ) %>%
  select(
    -wr_db_dist,
    -wr_fb_dist,
    -db_fb,
    -theta_fb_db_wr,
    -closest_x,
    -closest_y,
    -wr_x,
    -wr_y,
    -fb_x,
    -fb_y,
    # -db_s,
    -closest_s_dir,
    -wr_s_dir,
    # -wr_next_x,
    # -wr_next_y
  )
  
  # view(tot %>% head(25))
  
  frame <- frame %>% left_join(tot, all.x = TRUE)
  
  
  return(frame)
}

#This makes sure its only trained off of the arrival frame
arrival <- all %>% filter(frameId == arrival_frame)

#split into training and testing sets
ind <- sample(2, nrow(arrival), replace = TRUE, prob = c(0.8, 0.2))
train <- arrival[ind == 1,] #80%
test <- arrival[ind == 2,] #20%

#Simple logistic regression
naive <- glm(completed ~ closest_dist + dist + theta + sideline_dist + proj_s_per, family = "binomial", data = train)
summary(naive)

ncomp <- roc(test$completed, predict(naive, newdata = test))
ncomp_auc <- toString(ncomp$auc)
ncomp_auc #print out the AUC

#Apply it over the whole dataset
all$proj_comp <- predict(naive, newdata = all)
all$proj_comp_percent <- exp(all$proj_comp)/(1+exp(all$proj_comp))

#This is for if you want to look at the top/bottom players
players <- all %>% group_by(closest) %>% 
  summarise(
    frames = n(), 
    xcomp = mean(proj_comp_percent, na.rm = T),
    avg_sep = mean(closest_dist, na.rm = T),
    avg_downfield = mean(dist, na.rm = T),
    avg_leverage = mean(theta, na.rm = T),
    avg_dir_speed = mean(proj_s_per, na.rm = T),
    team = defensiveTeam,
    snaps = n_distinct(playId),
    games = n_distinct(gameId)
    ) %>% distinct()

