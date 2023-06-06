require(here)
require(magrittr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
require(wrapr)

#Data readin ----

Games <- read.csv(here('Data', 'game_info.csv'))
Players <- read.csv(here('Data', 'player_info.csv'))
Teams <- read.csv(here('Data', 'team_info.csv'))


game_ids <- unique(Games$gameid)
Events <- data.frame()
for (game in game_ids) {
  temp <- read.csv(here('Data', 'games', paste0(game, '-events.csv'))) %>%
    mutate(gameid = game) %>%
    select(gameid, period:expectedgoals)
  if (nrow(Events) == 0) {
    Events <- temp
  } else {
    Events <- rbind(Events, temp)
  }
}
Events <- Events %>%
  mutate(eventid = seq(1, nrow(.))) %>%
  select(eventid, gameid:expectedgoals)


#seeing if controlled entry events always preceed controlledentryagainst events
ControlledEntryAgainst <- Events %>%
  filter(name == 'controlledentryagainst' &
           type == '2on1') %$%
  eventid

#no, it isn't always controlledentryagainst
table(Events[ControlledEntryAgainst-1,'name'])

# Entry events ----
Controlled_2on1 <- Events %>%
  filter(name == 'controlledentryagainst' &
           type == '2on1' &
           skatersonicesituation == '5v5')

plays_followings_2on1 <- data.frame()
Entry_ID <- 1
for(row in (1:nrow(Controlled_2on1))) {
  temp <- Controlled_2on1[row,]
  
  SamePeriod <- Events %>%
    filter(gameid == temp$gameid &
             period == temp$period &
             gametime >= temp$gametime - 2 &
             gametime <= temp$gametime + 30)
  
  ControlledEntryBefore <- SamePeriod %>%
    filter(gametime <= temp$gametime &
             name == 'controlledentry')
  
  events_following <- SamePeriod %>%
    filter(gametime >= temp$gametime) %>%
    rbind(ControlledEntryBefore, .) %>%
    mutate(OffensiveTeamID = temp$opposingteamid,
           DefendingTeamID = temp$teamid,
           EntryID = Entry_ID,
           TimeSinceEntry = gametime - temp$gametime)
  
  plays_followings_2on1 <- rbind(plays_followings_2on1, events_following)
  Entry_ID <- Entry_ID + 1
}

plays_followings_2on1 <- plays_followings_2on1 %>%
  mutate(Team = if_else(teamid == OffensiveTeamID, 'Offensive', 'Defensive')) %>%
  left_join(select(Teams, teamid, teamshorthand), by='teamid')

PlaysFollowing5Sec <- plays_followings_2on1 %>%
  filter(TimeSinceEntry <= 5)

PlaysFollowing3Sec <- plays_followings_2on1 %>%
  filter(TimeSinceEntry <= 3)


#Graphing 2on1 sequence ----

entry_graphing <- function(dataset, Entry_ID) {
  entry <- filter(dataset, EntryID == Entry_ID)
  ggplot(entry, aes(x = xcoord, y = ycoord, color = teamshorthand, group = playerid, label = name)) + 
    geom_line() + geom_text() + geom_vline(xintercept = 25, color = 'blue', size = 2) +
    geom_vline(xintercept = 0, color = 'red', size = 2) + theme_classic() + coord_fixed() +
    geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)
}

entry_graphing(PlaysFollowing5Sec, 5)

#grouping some information up to the EntryID level ----
ParametricTimeframeFunction <- function(dataset, min_timeframe, max_timeframe) {
  dataset <- filter(dataset,TimeSinceEntry >= min_timeframe & TimeSinceEntry <= max_timeframe)
  #EventOrder for Attacking team
  EventOrder <- dataset %>%
    filter(Team == 'Offensive') %>%
    mutate(EventNumber = ave(gametime, EntryID, FUN = seq_along),
           CombinedEvent = paste(outcome, type, name),
           CombinedEvent = paste0('(', round(TimeSinceEntry, 2), ') ', CombinedEvent)) %>%
    select(EntryID, EventNumber, CombinedEvent) %>%
      pivot_wider(names_from = EventNumber, values_from = CombinedEvent)
  
  has_pass <- dataset %>%
    group_by(EntryID) %>%
    summarise(SuccessfulPass = sum(name == 'pass' & outcome == 'successful' & Team == 'Offensive'),
              AttemptedPass = sum(name == 'pass' & Team == 'Offensive'),
              BlockedPass = sum(name == 'block' & type == 'pass' & outcome == 'successful' & Team == 'Defensive'),
              Shot = sum(name == 'shot'),
              ExpGoalSum = sum(expectedgoals, na.rm = T),
              Goal = sum(name == 'goal' & Team == 'Offensive'))
  
  #Ordering pass events by Success or Failure:
  PassSuccessFailure <- dataset %>%
    filter(name == 'pass' & Team == 'Offensive')
  
  pass <- 1
  currentEntryID <- PassSuccessFailure[1, 'EntryID']
  Pass_Ids <- numeric(nrow(PassSuccessFailure))
  for(row in (1:nrow(PassSuccessFailure))) {
    if(PassSuccessFailure[row, 'EntryID'] != currentEntryID) {
      pass <- 1
      currentEntryID <- PassSuccessFailure[row, 'EntryID']
    }
    Pass_Ids[row] <- pass
    pass <- pass + 1
  }
  PassSuccessFailure <- PassSuccessFailure %>%
    mutate(PassNumber = Pass_Ids,
           Extra = case_when(PassNumber %% 100 == 11 ~ 'th',
                             PassNumber %% 100 == 12 ~ 'th',
                             PassNumber %% 100 == 13 ~ 'th',
                             PassNumber %% 10 == 1 ~ 'st',
                             PassNumber %% 10 == 2 ~ 'nd',
                             PassNumber %% 10 == 3 ~ 'rd',
                             TRUE ~ 'th'),
           PassNumber = paste0('Data_', PassNumber, Extra)) %>%
    select(EntryID, PassNumber, outcome) %>%
    pivot_wider(id_cols = EntryID, names_from = PassNumber, values_from = outcome) %>%
    left_join(select(has_pass, EntryID), ., by = 'EntryID')
  
  ProportionGoals <- has_pass %>%
    group_by(SuccessfulPass) %>%
    summarise(SumGoals = sum(Goal),
              Count = n()) %>%
    mutate(Proportion = SumGoals/Count)
  
  #Success Rate of Passes on 2on1 (unfinished)
  PassProbabilities <- data.frame()
  ForProbs <- select(PassSuccessFailure, -EntryID)
  for(column in names(ForProbs)) {
    number <- as.numeric(str_sub(column, 6, -3))
    temp <- ForProbs[,1:number]
    temp <- let(alias = list(rname = column), expr = (filter(temp, !is.na(rname))))
    #checking successful passes prior
    for(i in (1:number)[-number]) {
      if(nrow(temp) == 0) {break}
      ToFilter <- as.vector(temp[,i] == 'successful')
      temp <- temp[ToFilter,]
    }
    temp <- let(alias = list(rname = column), 
                expr = summarise(temp, Success = sum(rname == 'successful'), Count = n())) %>%
      mutate(ConditionalProbability = Success/Count,
             Type = str_sub(column, 6))
    PassProbabilities <- rbind(PassProbabilities, temp)
  }
  
  list(Reduced = dataset, EventOrder = EventOrder, has_pass = has_pass, 
       PassSuccessFailure = PassSuccessFailure, ProportionGoals = ProportionGoals, 
       PassProbabilities = PassProbabilities)
}

Seconds3 <- ParametricTimeframeFunction(plays_followings_2on1, -1, 3)
PlaysFollowing2on1_3Seconds <- Seconds3$Reduced
EventOrder <- Seconds3$EventOrder
has_pass <- Seconds3$has_pass
PassSuccessFailure <- Seconds3$PassSuccessFailure
ProportionGoals <- Seconds3$ProportionGoals
PassProbabilities <- Seconds3$PassProbabilities
rm(Seconds3)


PlaysFollowing2on1_3Seconds %>%
  filter(Team == 'Offensive' & name == 'shot') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord)) + geom_density2d() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

PlaysFollowing2on1_3Seconds %>%
  filter(Team == 'Offensive' & name == 'pass') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord)) + geom_density2d() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

PlaysFollowing2on1_3Seconds %>%
  filter(Team == 'Offensive' & name == 'goal') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord)) + geom_density2d() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)


entry_graphing(PlaysFollowing2on1_3Seconds, 10)


#testing speed of possession player

plays_followings_2on1 %>%
  filter(TimeSinceEntry >= -5, TimeSinceEntry <= 3) %>%
  entry_graphing(702)
