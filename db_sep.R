library(tidyverse)
source("https://raw.githubusercontent.com/dutta/NFL3D/master/add_z.R")
library(ggplot2)
library(ggrepel)
library(pROC)
library(gganimate)

games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")
targets <- read.csv("targetedReciever.csv")

# wk1 <- read.csv("week1.csv")
# wk2 <- read.csv("week2.csv")
# wk3 <- read.csv("week3.csv")
# wk4 <- read.csv("week4.csv")
# wk5 <- read.csv("week5.csv")
# wk6 <- read.csv("week6.csv")
# wk7 <- read.csv("week7.csv")
# wk8 <- read.csv("week8.csv")
# wk9 <- read.csv("week9.csv")
# wk10 <- read.csv("week10.csv")
# wk11 <- read.csv("week11.csv")
# wk12 <- read.csv("week12.csv")
# wk13 <- read.csv("week13.csv")
# wk14 <- read.csv("week14.csv")
# wk15 <- read.csv("week15.csv")
# wk16 <- read.csv("week16.csv")
# wk17 <- read.csv("week17.csv")

#We'll start by just fucking around with week 1, then will expand later

all <- data.frame()

#You shouldn't have to increase the memory limit. the resulting data frame is ~800 MB
for(i in 1:17){
  print(paste0("Start Week ", toString(i)))
  
  wk1 <- read.csv(paste0("week",toString(i),".csv")) %>%
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  #These are the plays which throw errors
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
    mutate(arrival_frame = max(frameId[event %in% pass_arrival_events]), snap_frame = max(frameId[event == 'ball_snap'])) %>% 
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
  
  player_ids <- df_merged %>% select(displayName, nflId) %>% distinct() %>% rename("closest_wr" = "displayName", "closestId" = "nflId")
  
  df_merged <- df_merged %>% 
    left_join(player_ids, all.x = TRUE) %>% 
    mutate(
      completed = ifelse(closestId != targetNflId, 0, ifelse(passResult == 'C', 1, 0)),
      targeted = ifelse(frameId != arrival_frame, 0, ifelse(closestId == targetNflId, 1, 0))
      )
  
  #comment out this line if you want to animate a given play
  df_merged <- df_merged %>% filter(sideOfBall == 'defense') %>% filter(displayName != 'Football')
  
  df_merged$closestId[is.na(df_merged$closestId)] <- 0
  df_merged$targeted[is.na(df_merged$targeted)] <- 0
  df_merged$completed[is.na(df_merged$completed)] <- 0
  
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

#This is the main function that gets the geometric factors which go into the model
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
      wr_s = s,
      wr_a = a
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
    group_by(gameId, playId, frameId, db) %>% 
    summarise(
      closest_dist = min(pythag_dist),
      closest = wr[pythag_dist == closest_dist],
      closest_x = wr_x[wr == closest],
      closest_y = wr_y[wr == closest],
      closest_s = wr_s[wr == closest],
      closest_a = wr_a[wr == closest],
      closest_s_dir = wr_s_dir[wr == closest],
      db_s = db_s,
      db_s_dir = db_s_dir
    ) %>% 
    mutate(
      closest_wr = ifelse(closest_dist > 15, "Deep Zone", closest),
      sideline_dist = min(ifelse(closest_x < 60, closest_x, 120 - closest_x), ifelse(closest_y < 53.333/2, closest_y, 53.333 - closest_y)),
      proj_s_per = ifelse(db_s == 0, 0, (db_s*(cos(closest_s_dir - db_s_dir)))/db_s)
    ) %>% 
    select(-closest) %>% 
    rename("displayName" = "db") %>%
    distinct() 
  
  tot <- tot %>% inner_join(fp_fb) %>% inner_join(fp_db %>% rename(displayName = db)) %>%
    group_by(gameId, playId) %>% 
    mutate(
      closest_fb_dist = sqrt((closest_x - fb_x)^2 + (closest_y - fb_y)^2),
      db_fb = sqrt((db_x - fb_x)^2 + (db_y - fb_y)^2),
      theta_fb_db_wr = acos((((db_x - fb_x) * (db_x - closest_x)) + ((db_y - fb_y) * (db_y - closest_y)))/(db_fb * closest_dist)),
      theta_fb_db_wr = (180/pi)*ifelse(theta_fb_db_wr > pi, theta_fb_db_wr - pi, theta_fb_db_wr)
    ) %>%
    ungroup() %>% 
    mutate(
      dist = closest_fb_dist,
      theta = theta_fb_db_wr,
    ) %>% 
    select(
      -closest_fb_dist,
      -db_fb,
      -theta_fb_db_wr,
      -closest_x,
      -closest_y,
      -db_x,
      -db_y,
      -fb_x,
      -fb_y,
      -db_s,
      -closest_s_dir,
      -db_s_dir,
      -db_next_x,
      -db_next_y
    )
  
  frame <- frame %>% left_join(tot, all.x = TRUE)
  
  
  return(frame)
}

#This makes sure its only trained off of the arival frame on targeted WRs
arrival <- all %>% filter(frameId == arrival_frame & targeted == 1)

ind <- sample(2, nrow(arrival), replace = TRUE, prob = c(0.8, 0.2))
train <- arrival[ind == 1,] #80%
test <- arrival[ind == 2,] #20%

#Simple logistic regression
naive <- glm(completed ~ closest_dist + dist + theta + sideline_dist + proj_s_per + s_diff + closest_a, family = "binomial", data = train)
summary(naive)

ncomp <- roc(test$completed, predict(naive, newdata = test))
ncomp_auc <- toString(ncomp$auc)
ncomp_auc

all$proj_comp <- predict(naive, newdata = all)
all$proj_comp_percent <- 100*exp(all$proj_comp)/(1+exp(all$proj_comp))

#This stuff is only for if you want to animate a play
# def <- all %>% filter(sideOfBall == 'defense') %>% filter(displayName != 'Football')
# off <- all %>% filter(sideOfBall != 'defense')
# ball <- all %>% filter(displayName == 'Football')
# 
# off$proj_comp <- 0 
# off$proj_comp_percent <- 0.5 This value is arbitrary so that the o players/ball have a dot in the animation
# 
# ball$proj_comp <- 0
# ball$proj_comp_percent <- 0.5
# 
# test$proj_comp <- predict(naive, newdata = test)
# test$proj_comp_percent <- 100*exp(test$proj_comp)/(1+exp(test$proj_comp))
# 
# 
# def$proj_comp <- predict(naive, newdata = def)
# def$proj_comp_percent <- 100*exp(def$proj_comp)/(1+exp(def$proj_comp))
# 
# all <- bind_rows(off, ball) %>% bind_rows(def)

#this line is for if we actually want to try the db contribution idea
# all$db_cont <- 100*(exp(all$closest_dist * naive$coefficients[2])/(1+exp(all$closest_dist * naive$coefficients[2]))) + 100*(exp(all$theta * naive$coefficients[4])/(1+exp(all$theta * naive$coefficients[4]))) #+ 100*(exp(all$o * naive$coefficients[9])/(1+exp(all$o * naive$coefficients[9])))+ 100*(exp(all$dir * naive$coefficients[10])/(1+exp(all$dir * naive$coefficients[10])))

#This is for if you want to look at the top/bottom players
players <- all %>% filter(closest_wr != 'Deep Zone') %>% group_by(displayName) %>% 
  summarise(
    position = position, 
    frames = n(), 
    xcomp = mean(proj_comp_percent, na.rm = T),
    avg_sep = mean(closest_dist, na.rm = T),
    avg_downfield = mean(dist, na.rm = T),
    avg_leverage = mean(theta, na.rm = T),
    avg_dir_speed = mean(proj_s_per, na.rm = T),
    avg_closest_a = mean(closest_a, na.rm = T),
    targets = sum(targeted),#i am unsure if this is the right way to do it
    team = defensiveTeam
    ) %>% distinct()

#This is just a fun distribution plot

ggplot(players %>% filter(targets > 10))+
  geom_histogram(mapping = aes(x = xcomp))+
  labs(title = "Distributions of players' average xComp min. 10 targets")

ggsave("distribution.png", height = 7, width = 13)

#This gets the data from the WR perspective

wrs <- all %>% filter(closest_wr != 'Deep Zone') %>% group_by(closest_wr) %>% summarise(xcomp = mean(proj_comp_percent, na.rm = T), frames = n(),
                                                                                        avg_separation = mean(closest_dist, na.rm = T),
                                                                                        avg_downfield = mean(dist, na.rm = T),
                                                                                        avg_leverage = mean(theta, na.rm = T), 
                                                                                        avg_s_dir = mean(proj_s_per, na.rm = T), 
                                                                                        targets = sum(targeted)#This definitely doesn't give the right number of WR targets) %>% distinct()


#This is the animation section. You need to comment out the defense only filter in the main loop to run this

# rand_play <- all %>% 
#   select(gameId, playId) %>% 
#   sample_n(1) %>% 
#   inner_join(all, by = c('gameId','playId')) %>% 
#   mutate(
#     shown_name = case_when(
#       displayName == 'Football' ~ '',
#       sideOfBall == 'defense' ~ closest_wr,
#       sideOfBall == 'offense' ~ displayName,
#       TRUE ~ 'man idk'
#     )
#   )
# 
# ggplot(data = rand_play)+
#   geom_point(mapping = aes(x = x, y = y, color = sideOfBall, size = proj_comp_percent))+
#   geom_text(mapping = aes(x = x, y = y, label = shown_name))+
#   labs(title = paste("Result", rand_play$passResult[1], sep = " "))+
#   transition_time(frameId)
# 
# anim_save("rand_play.gif")

#This is the ROC curve

ggncomp_roc <- ggroc(ncomp)

ggncomp_roc+
  geom_text(mapping = aes(x = 0.5, y = 0.5, label = paste0('AUC of ', ncomp_auc)))+
  labs(title = "Naive Separation-based Completion Model ROC Curve",
       caption = "Data from Big Data Bowl | By: Conor McQuiston @ConorMcQ5")+
  theme(plot.title = element_text(hjust = 0.5, family = "Tahoma", size = 15),
        plot.caption = element_text(family = "Tahoma"))+
  theme(panel.background = element_rect(fill = "seashell", color = "gray", size = 0.5, linetype = "solid"))+
  theme(plot.background = element_rect(fill = "seashell"))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid.minor=element_blank())+
  theme(plot.title = element_text(hjust = 0.5, family = "Tahoma", size = 15),
        plot.subtitle = element_text(hjust = 0.5, family = "Tahoma", size = 13),
        plot.caption = element_text(family = "Tahoma"))+
  theme(axis.title.y = element_text(size = 13, family = "Tahoma"),
        axis.text.x = element_text(size = 11, family = "Tahoma"),
        axis.text.y = element_text(size = 11, family = "Tahoma"))

ggsave("sep_roc.png", dpi = 300)