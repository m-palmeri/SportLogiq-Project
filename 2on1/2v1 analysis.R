require(here)
require(dplyr)
require(magrittr)
require(ggplot2)
require(tidyr)

#Data readin  and functions----

Games <- read.csv(here('Data', 'game_info.csv'))
Players <- read.csv(here('Data', 'player_info.csv'))
Teams <- read.csv(here('Data', 'team_info.csv'))
Events <- read.csv(here('Data', 'compiled_events.csv'))


entry_graphing <- function(dataset, Entry_ID) {
  entry <- filter(dataset, EntryID == Entry_ID)
  ggplot(entry, aes(x = xcoord, y = ycoord, color = teamshorthand, group = playerid, label = name)) + 
    geom_line() + geom_text() + geom_vline(xintercept = 25, color = 'blue', size = 2) +
    geom_vline(xintercept = 0, color = 'red', size = 2) + theme_classic() + coord_fixed() +
    geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)
}

prior_plays_graphing <- function(dataset, x_variable, goal_breakdown = F) {
  index <- grep(x_variable, colnames(dataset))
  colnames(dataset)[index] <- 'x_var'
  if(goal_breakdown){
    dataset <- mutate(dataset, Goal = if_else(Goal == 1, 'Yes', 'No')) %>%
      group_by(x_var) %>%
      summarise(Yes = sum(Goal == 'Yes'),
                No = sum(Goal == 'No')) %>%
      mutate(Total = Yes + No,
             probability = Yes/Total) %>%
      pivot_longer(names_to = 'Goal', values_to = 'Count', cols = c(Yes, No))
    ggplot(dataset, aes(x = x_var, y = Count, fill = Goal, label = paste0(round(probability*100, 2), '%'))) +
      geom_bar(stat = 'identity', position = position_stack(reverse = T)) + theme_bw() + geom_text(position = position_stack(vjust=1.2)) +
      labs(x = x_variable) + scale_fill_manual(values = c('Dark Red', 'Dark Green'))
  }else{
    ggplot(dataset, aes(x = x_var)) + geom_bar() + theme_bw() + labs(x = x_variable)
  }
}
#simple two proportion test
Event_Probability_Difference <- function(dataset, variable) {
  index <- grep(variable, colnames(dataset))
  colnames(dataset)[index] <- 'x_var'
  test <- dataset %>%
    mutate(x_var = if_else(x_var == 0, 0, 1),
           Goal = if_else(Goal == 1, 'Yes', 'No')) %>%
    group_by(x_var, Goal) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(total = ave(count, x_var, FUN=sum))
  p_value <- test %>%
    filter(Goal == 'Yes') %>%
    select(count, total)
  prop.test(p_value$count, p_value$total)
}

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
             gametime >= temp$gametime - 15 &
             gametime <= temp$gametime + 30)
  
  events_following <- SamePeriod %>%
    mutate(OffensiveTeamID = temp$opposingteamid,
           DefendingTeamID = temp$teamid,
           EntryID = Entry_ID,
           TimeSinceEntry = gametime - temp$gametime)
  
  plays_followings_2on1 <- rbind(plays_followings_2on1, events_following)
  print(Entry_ID)
  Entry_ID <- Entry_ID + 1
}
rm(temp)
rm(Entry_ID)
rm(Controlled_2on1)
rm(SamePeriod)
rm(events_following)
rm(row)

shot_faceoff_time <- plays_followings_2on1 %>%
  filter(TimeSinceEntry >= 0) %>%
  filter(name == 'shot' | name == 'faceoff') %>%
  group_by(EntryID, name) %>%
  summarise(min_time = min(TimeSinceEntry)) %>%
  pivot_wider(names_from = name, values_from = min_time)

plays_followings_2on1 <- plays_followings_2on1 %>%
  mutate(Team = if_else(teamid == OffensiveTeamID, 'Offensive', 'Defensive')) %>%
  left_join(select(Teams, teamid, teamshorthand), by='teamid') %>%
  left_join(shot_faceoff_time, by='EntryID') %>%
  mutate(shot = if_else(is.na(shot), 30, shot),
         faceoff = if_else(is.na(faceoff), 30, faceoff))


#Creating Entry_Information dataset ----
#4 second cut-off after entry
Entry_Information <- plays_followings_2on1 %>%
  filter(TimeSinceEntry >= 0 & TimeSinceEntry <= 4 &
           TimeSinceEntry <= shot + 1 & TimeSinceEntry <= faceoff) %>%
  mutate(ShotBlock = case_when(name == 'shot' & grepl('blocked', type) ~ 'blocked',
                               name == 'shot' ~ 'notblocked',
                               TRUE ~ NA_character_),
         ShotSlot = case_when(name == 'shot' & grepl('slot', type) ~ 'slot',
                              name == 'shot' ~ 'outside',
                              TRUE ~ NA_character_)) %>%
  group_by(EntryID) %>%
  summarise(Goal = sum(name == 'goal' & Team == 'Offensive'),
            AttemptedPasses = sum(name == 'pass' & Team == 'Offensive'),
            SuccessfulPasses = sum(name == 'pass' & outcome == 'successful' & Team == 'Offensive'),
            AttemptedPassBlocks = sum(name == 'block' & type == 'pass' & Team == 'Defensive'),
            BlockedPasses = sum(name == 'block' & type == 'pass' & outcome == 'successful' & Team == 'Defensive'),
            AttemptedShotBlocks = sum(name == 'block' & type == 'shot' & Team == 'Defensive'),
            SuccessfulShots = sum(name == 'shot' & outcome == 'successful' & Team == 'Offensive' & ShotBlock == 'notblocked'),
            MissedShots = sum(name == 'shot' & outcome == 'failed' & Team == 'Offensive' & ShotBlock == 'notblocked'),
            BlockedShots = sum(name == 'shot' & Team == 'Offensive' & ShotBlock == 'block'),
            SuccessfulSlotShots = sum(name == 'shot' & outcome == 'successful' & Team == 'Offensive' & ShotBlock == 'notblocked' & ShotSlot == 'slot'),
            MissedSlotShots = sum(name == 'shot' & outcome == 'failed' & Team == 'Offensive' & ShotBlock == 'notblocked' & ShotSlot == 'slot'),
            BlockedSlotShots = sum(name == 'shot' & Team == 'Offensive' & ShotBlock == 'block' & ShotSlot == 'slot'),
            ExpectedGoals = sum(expectedgoals, na.rm=T))


#Location of Possession Player
entry_player_id <- plays_followings_2on1 %>%
  filter(TimeSinceEntry < 0.5 & TimeSinceEntry > -0.5 & Team == 'Offensive' &
           xadjcoord >= 20 & xadjcoord <= 30 & (name == 'controlledentry' | name == 'carry'))

PossessionPlayerLocation <- entry_player_id %>%
  group_by(EntryID) %>%
  summarise(first_val = min(eventid)) %>%
  mutate(for_join = paste(EntryID, first_val)) %>%
  select(for_join) %>%
  inner_join(mutate(entry_player_id, for_join = paste(EntryID, eventid)), by='for_join') %>%
  select(EntryID, playerid, xcoord, ycoord, teamid) %>%
  rename(Possessionplayerid = playerid,
         PossessionXCoord = xcoord,
         PossessionYCoord = ycoord,
         PossessionTeamID = teamid)

Entry_Information <- inner_join(Entry_Information, PossessionPlayerLocation, by='EntryID')

#Handedness on Possession Player
PossessionPlayerInformation <- entry_player_id %>%
  group_by(EntryID) %>%
  summarise(playerid = unique(playerid)) %>%
  inner_join(Players, by='playerid') %>%
  select(EntryID, primaryPosition, handedness) %>%
  rename(PossessionPosition = primaryPosition,
         PossessionHandedness = handedness)

Entry_Information <- inner_join(Entry_Information, PossessionPlayerInformation, by='EntryID')

rm(PossessionPlayerInformation)
#Speed of Possession Player on Entry (if applicable):
PreviousPlayLocation <- entry_player_id %>%
  group_by(EntryID) %>%
  summarise(playerid = unique(playerid),
            EntryTime = unique(gametime)) %>%
  mutate(for_join = paste(EntryID, playerid)) %>%
  select(for_join, EntryTime) %>%
  inner_join(mutate(plays_followings_2on1, for_join = paste(EntryID, playerid)), by='for_join') %>%
  mutate(TimeSincePossessionEntry = gametime - EntryTime) %>%
  filter(TimeSincePossessionEntry < 0) %>%
  mutate(LatestEventTime = ave(TimeSincePossessionEntry, EntryID, FUN=max),
         ChangeTime = abs(TimeSincePossessionEntry)) %>%
  filter(TimeSincePossessionEntry == LatestEventTime) %>%
  select(EntryID, gametime, xcoord, ycoord, ChangeTime) %>%
  rename(PreviousPlayGametime = gametime,
         PreviousPlayxcoord = xcoord,
         PreviousPlayycoord = ycoord)

SpeedPossession <- PreviousPlayLocation %>%
  inner_join(PossessionPlayerLocation, by='EntryID') %>%
  mutate(ChangePosition = sqrt((PreviousPlayxcoord - PossessionXCoord)^2 + 
                                 (PreviousPlayycoord - PossessionYCoord)^2),
         Speed = ChangePosition/ChangeTime) %>%
  select(EntryID, ChangePosition, ChangeTime, Speed) %>%
  rename(PreEntryChangePosition = ChangePosition,
         PreEntryChangeTime = ChangeTime,
         PreEntrySpeed = Speed)

Entry_Information <- inner_join(Entry_Information, SpeedPossession, by='EntryID')

rm(SpeedPossession)
rm(PreviousPlayLocation)
rm(PossessionPlayerLocation)
rm(entry_player_id)
#Properties of Defending player
DefendingPlayerInformation <- plays_followings_2on1 %>%
  filter(TimeSinceEntry == 0 & name == 'controlledentryagainst') %>%
  inner_join(Players, by='playerid') %>%
  select(EntryID, playerid, xcoord, ycoord, primaryPosition, handedness, teamid) %>%
  rename(Defendingplayerid = playerid,
         DefendingXCoord = xcoord,
         DefendingYCoord = ycoord,
         DefendingPosition = primaryPosition,
         DefendingHandedness = handedness,
         DefendingTeamID = teamid) %>%
  filter(DefendingPosition != 'G')

Entry_Information <- inner_join(Entry_Information, DefendingPlayerInformation, by='EntryID')

rm(DefendingPlayerInformation)

#Plays that lead to each 2on1
PreemptivePlays <- plays_followings_2on1 %>%
  filter(TimeSinceEntry >= -5 & TimeSinceEntry < 0) %>%
  mutate(StretchPass = if_else(grepl('stretch', type), 'Yes', 'No'),
         OutletPass = if_else(grepl('outlet', type), 'Yes', 'No')) %>%
  group_by(EntryID) %>%
  summarise(`Failed Passes Defensive` = sum(name == 'pass' & outcome == 'failed' & Team == 'Defensive'),
            `LPR Offensive Won` = sum(name == 'lpr' & outcome == 'successful' & Team == 'Offensive'),
            `Pass Block Offensive` = sum(name == 'block' & type == 'pass' & outcome == 'successful' & Team == 'Offensive'),
            `Body Check Offensive` = sum(name == 'check' & type == 'body' & outcome == 'successful' & Team == 'Offensive'),
            `Stick Check Offensive` = sum(name == 'check' & type == 'stick' & outcome == 'successful' & Team == 'Offensive'),
            `Stretch Pass Fail Defensive` = sum(name == 'pass' & StretchPass == 'Yes' & outcome == 'failed' & Team == 'Defensive'),
            `Outlet Pass Fail Defensive` = sum(name == 'pass' & OutletPass == 'Yes' & outcome == 'failed' & Team == 'Defensive'),
            `Passes Success Offensive` = sum(name == 'pass' & outcome == 'successful' & Team == 'Offensive'),
            `Stretch Pass Success Offensive` = sum(name == 'pass' & StretchPass == 'Yes' & outcome == 'successful' & Team == 'Offensive'),
            `Outlet Pass Success Offensive` = sum(name == 'pass' & OutletPass == 'Yes' & outcome == 'successful' & Team == 'Offensive'))

Entry_Information <- inner_join(Entry_Information, PreemptivePlays, by='EntryID')


#adding other variables of interest:
Entry_Information <- Entry_Information %>%
  filter(abs(DefendingXCoord) >= 25 & abs(DefendingXCoord) <= 50) %>%
  mutate(X_Distance = abs(PossessionXCoord - DefendingXCoord),
         Y_Distance = abs(PossessionYCoord - DefendingYCoord),
         Distance = sqrt(X_Distance^2 + Y_Distance^2),
         AttackingSide = case_when(DefendingXCoord < 0 & PossessionYCoord > DefendingYCoord ~ 'R',
                                   DefendingXCoord > 0 & PossessionYCoord < DefendingYCoord ~ 'R',
                                   TRUE ~ 'L'),
         AttackingSameHandedness = if_else(AttackingSide == PossessionHandedness, 1, 0),
         DefendingSameHandedness = if_else(AttackingSide == DefendingHandedness, 1, 0),
         AttemptPassBoolean = if_else(AttemptedPasses > 0, 1, 0),
         SuccessfulPassBoolean = if_else(SuccessfulPasses > 0, 1, 0),
         BlockedPassBoolean = if_else(BlockedPasses > 0, 1, 0),
         AttemptedShots = SuccessfulShots + BlockedShots + MissedShots,
         AttemptShotBoolean = if_else(AttemptedShots > 0, 1, 0))


#testing logistic regressions: ----
Goal_SuccessfulPass <- glm(Goal ~ SuccessfulPassBoolean + Distance + AttackingSameHandedness +
                                   PreEntrySpeed + DefendingSameHandedness, 
                                 data = Entry_Information, family='binomial')
summary(Goal_SuccessfulPass)


Pass_Shot_Relation <- glm(AttemptShotBoolean ~ AttemptPassBoolean + PreEntrySpeed + Distance + 
                            DefendingSameHandedness + AttackingSameHandedness, data = Entry_Information, family='binomial')
summary(Pass_Shot_Relation)


Goal_AttemptedPass <- Entry_Information %>%
  filter(Distance < 30) %>%
  glm(Goal ~ AttemptPassBoolean + AttackingSameHandedness +
                            PreEntrySpeed + DefendingSameHandedness, 
                          data = ., family='binomial')
summary(Goal_AttemptedPass)


Successful_Pass_Relation <- glm(SuccessfulPassBoolean ~ Distance + DefendingSameHandedness + PreEntrySpeed +
                                  AttackingSameHandedness, data = Entry_Information, family = 'binomial')
summary(Successful_Pass_Relation)

#Plotting location of shots, passes on 2on1

#passes
plays_followings_2on1 %>%
  filter(Team == 'Offensive' & name == 'pass' & TimeSinceEntry >= 0 & TimeSinceEntry <= 3 & xadjcoord > 25) %>%
  mutate(xnewcoord = if_else(outcome == 'successful', xadjcoord, -xadjcoord),
         ynewcoord = if_else(outcome == 'successful', yadjcoord, -yadjcoord)) %>%
  ggplot(aes(x = xnewcoord, y = ynewcoord, color = outcome)) + geom_point() + theme_classic() + coord_fixed() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) + geom_vline(xintercept = 0, color = 'red', size = 2) + 
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

#shots
plays_followings_2on1 %>%
  filter(Team == 'Offensive', name == 'shot' & TimeSinceEntry >= 0 & TimeSinceEntry <= 3) %>%
  mutate(xnewcoord = if_else(outcome == 'successful', xadjcoord, -xadjcoord),
         ynewcoord = if_else(outcome == 'successful', yadjcoord, -yadjcoord)) %>%
  ggplot(aes(x = xnewcoord, y = ynewcoord, color = outcome)) + geom_point() + theme_classic() + coord_fixed() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) + geom_vline(xintercept = 0, color = 'red', size = 2) + 
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)


#Team Attacking Proficiency on 2on1 ----
Team_Possession <- Entry_Information %>%
  rename(teamid = PossessionTeamID) %>%
  inner_join(Teams, by='teamid') %>%
  mutate(TeamName = paste(teamlocation, teamname)) %>%
  group_by(TeamName) %>%
  summarise(PossessionCount2on1 = n(),
            PossessionGoals = sum(Goal),
            PossessionNumberWithShot = sum(AttemptShotBoolean),
            PossessionNumberWithPass = sum(AttemptPassBoolean),
            PossessionGoalWithPass = sum(AttemptPassBoolean == 1 & Goal == 1),
            PossessionNumberWithoutPass = sum(AttemptPassBoolean == 0),
            PossessionGoalWithoutPass = sum(AttemptPassBoolean == 0 & Goal == 1)) %>%
  mutate(PossessionProbabilityGoal = PossessionGoals/PossessionCount2on1,
         PossessionProbabilityWithPass = PossessionNumberWithPass/PossessionCount2on1,
         PossessionProbabilityGoalWithPass = PossessionGoalWithPass/PossessionNumberWithPass,
         PossessionProbabilityGoalWithoutPass = PossessionGoalWithoutPass/PossessionNumberWithoutPass,
         PossessionDifference = PossessionProbabilityGoalWithPass - PossessionProbabilityGoalWithoutPass)
  
Team_Possession %>%
  select(TeamName, PossessionProbabilityGoalWithPass, PossessionProbabilityGoalWithoutPass) %>%
  pivot_longer(cols = contains('Probability'), names_to = 'PassIndicator', values_to = 'Probability') %>%
  ggplot(aes(x = PassIndicator, y = Probability)) + geom_boxplot() + geom_jitter() + geom_point(color = 'red')

#Team Defending Proficiency on 2on1 ----
Team_Defending <- Entry_Information %>%
  rename(teamid = DefendingTeamID) %>%
  inner_join(Teams, by='teamid') %>%
  mutate(TeamName = paste(teamlocation, teamname)) %>%
  group_by(TeamName) %>%
  summarise(DefendingCount2on1 = n(),
            DefendingGoals = sum(Goal),
            DefendingNumberWithShot = sum(AttemptShotBoolean),
            DefendingNumberWithPass = sum(AttemptPassBoolean),
            DefendingGoalWithPass = sum(AttemptPassBoolean == 1 & Goal == 1),
            DefendingNumberWithoutPass = sum(AttemptPassBoolean == 0),
            DefendingGoalWithoutPass = sum(AttemptPassBoolean == 0 & Goal == 1)) %>%
  mutate(DefendingProbabilityGoal = DefendingGoals/DefendingCount2on1,
         DefendingProbabilityWithPass = DefendingNumberWithPass/DefendingCount2on1,
         DefendingProbabilityGoalWithPass = DefendingGoalWithPass/DefendingNumberWithPass,
         DefendingProbabilityGoalWithoutPass = DefendingGoalWithoutPass/DefendingNumberWithoutPass,
         DefendingDifference = DefendingProbabilityGoalWithPass - DefendingProbabilityGoalWithoutPass)


#Team Summary ----
Team_Summary <- Teams %>%
  mutate(TeamName = paste(teamlocation, teamname)) %>%
  select(TeamName, teamshorthand) %>%
  inner_join(Team_Possession, by='TeamName') %>%
  inner_join(Team_Defending, by='TeamName')

rm(Team_Defending)
rm(Team_Possession)

Team_Scoring <- Events %>%
  group_by(gameid, teamid) %>%
  summarise(goals = sum(name == 'goal')) %>%
  ungroup() 

ggplot(Team_Summary, aes(x = PossessionProbabilityGoal, y = DefendingProbabilityGoal, label = teamshorthand)) + 
  geom_point() + geom_hline(yintercept = 0.0926) + geom_vline(xintercept = 0.0926) + 
  geom_text(nudge_y = 0.005) + theme_classic() + scale_y_reverse() + xlab('Attacking Goal Probability') + 
  ylab('Defending Goal Probability') + ggtitle('Probability Comparison') + theme(plot.title = element_text(hjust=0.5)) 

ggplot(Team_Summary, aes(x = PossessionCount2on1, y = DefendingCount2on1, label = teamshorthand)) + 
  geom_point() + geom_hline(yintercept = 27.871) + geom_vline(xintercept = 27.871) + 
  geom_text(nudge_y = 0.75) + theme_classic() + scale_y_reverse() + xlab('Attacking 2on1 Count') + 
  ylab('Defending 2on1 Count') + ggtitle('Count Comparison') + theme(plot.title = element_text(hjust=0.5))
  


#Distance and pass/shot success
distances = seq(10, 45, by=0.25)
distances_df = data.frame()
for(x in distances) {
  temp <- Entry_Information %>%
    mutate(distance_seperator = if_else(Distance <= x, 'Less Than', 'Greater Than'),
           AttemptPassBoolean = if_else(AttemptPassBoolean == 1, 'With', 'Without')) %>%
    group_by(AttemptPassBoolean, distance_seperator) %>%
    summarise(count = n(),
              Goals = sum(Goal)) %>%
    ungroup() %>%
    mutate(total_count = ave(count, distance_seperator, FUN=sum),
           goal_odds = Goals/(count-Goals),
           combined = paste(distance_seperator, AttemptPassBoolean, 'Pass')) %>%
    select(combined, goal_odds, total_count)
  temp1 <- temp %>%
    select(combined, goal_odds) %>%
    pivot_wider(names_from = combined, values_from = goal_odds) %>%
    mutate(Distance = x) %>%
    .[, c(5, 1:4)]
  temp2 <- temp %>%
    .[c(1,2),] %>%
    select(-goal_odds) %>%
    mutate(combined = if_else(combined == 'Greater Than With Pass', 'Greater_Count', 'Less_Count')) %>%
    pivot_wider(names_from = combined, values_from = total_count)
  temp <- cbind(temp1, temp2)
  distances_df <- rbind(distances_df, temp)
}

rm(temp)
rm(distances)
rm(x)
rm(temp1)
rm(temp2)


#odds ratio have greater on top, or pass on top
distances_df <- distances_df %>%
  mutate(With_pass_odds_ratio = `Greater Than With Pass`/`Less Than With Pass`,
         Without_pass_odds_ratio = `Greater Than Without Pass`/`Less Than Without Pass`,
         Greater_than_odds_ratio = `Greater Than With Pass`/`Greater Than Without Pass`,
         Less_than_odds_ratio = `Less Than With Pass`/`Less Than Without Pass`,
         weighted_greater = `Greater Than Without Pass` - `Greater Than With Pass`,
         weighted_less = `Less Than With Pass` - `Less Than Without Pass`,
         weighted_diff = weighted_greater*Greater_Count + weighted_less*Less_Count)

ggplot(distances_df, aes(x = Distance, y = weighted_diff)) + geom_line()
ggplot(distances_df, aes(x = Distance, y = With_pass_odds_ratio)) + geom_point() + theme_bw()
ggplot(distances_df, aes(x = Distance, y = Without_pass_odds_ratio)) + geom_point() + theme_bw()
ggplot(distances_df, aes(x = Distance, y = Greater_than_odds_ratio)) + geom_point() + theme_bw()
ggplot(distances_df, aes(x = Distance, y = Less_than_odds_ratio)) + geom_point() + theme_bw()

#plotting better to pass below, and shot above on same axis
distances_df %>%
  select(Distance, Greater_than_odds_ratio, Less_than_odds_ratio) %>%
  pivot_longer(names_to = 'type', values_to = 'OddsRatio', cols = c(Greater_than_odds_ratio, Less_than_odds_ratio)) %>%
  ggplot(aes(x = Distance, y = OddsRatio, color = type)) + geom_line() + geom_point() + theme_bw() +
  geom_hline(yintercept = 1) + theme(legend.position = c(0.8, 0.8))


#bar chart with passing vs shooting with a threshold of 30 feet
Entry_Information %>%
  mutate(Distance_30 = Distance > 30) %>%
  group_by(Goal, AttemptPassBoolean, Distance_30) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(temp = paste(AttemptPassBoolean, Distance_30),
         total = ave(count, temp, FUN=sum),
         percent = round(100*count/total, 2)) %>%
  mutate(AttemptPassBoolean = if_else(AttemptPassBoolean == 0, 'No Pass Attempted', 'Pass Attempted'),
         Distance_30 = if_else(Distance_30, 'Greater Than 30 Feet', 'Less Than 30 Feet'),
         Distance_30 = as.factor(Distance_30),
         Distance_30 = factor(Distance_30, levels = c('Less Than 30 Feet', 'Greater Than 30 Feet')),
         percent = if_else(Goal == 1, paste0(percent, '%'), NA_character_),
         Goal = if_else(Goal == 1, 'Yes', 'No')) %>%
  ggplot(aes(x = AttemptPassBoolean, y = count, fill = Goal)) + theme_bw() +
  geom_bar(stat = 'identity', position = position_stack()) + facet_grid( ~ Distance_30) +
  geom_text(aes(label = percent, y = total + 25)) + scale_fill_manual(values = c('Light Blue', 'Dark Blue')) + 
  labs(x = 'Pass Attempted?', y = 'Number of Entries', fill = 'Goal?')


#Total Distance
Passing_DistanceBuckets <- Entry_Information %>%
  mutate(Distance_Buckets = case_when(Distance < 15 ~ '0-15',
                                      Distance < 25 ~ '15-25',
                                      Distance < 30 ~ '25-30',
                                      Distance < 35 ~ '30-35',
                                      Distance < 45 ~ '35-45',
                                      Distance > 45 ~ '45-60'),
         Distance_Buckets = as.factor(Distance_Buckets),
         Distance_Buckets = factor(Distance_Buckets, levels = c('0-15', '15-25', '25-30', 
                                                                '30-35', '35-45', '45-60'))) %>%
  group_by(Distance_Buckets, AttemptPassBoolean, Goal) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(temp = paste(Distance_Buckets, AttemptPassBoolean),
         total = ave(count, temp, FUN=sum)) %>%
  select(-temp) %>%
  filter(Goal == 0) %>%
  mutate(Goal = total-count,
         NoGoal = count,
         odds = Goal/NoGoal,
         Total_Goals = ave(Goal, Distance_Buckets, FUN=sum),
         Total_No_Goals = ave(NoGoal, Distance_Buckets, FUN=sum)) %>%
  select(-c(count, total, Goal, NoGoal)) %>%
  mutate(AttemptPassBoolean = if_else(AttemptPassBoolean == 0, 'No_Pass_Attempted_Odds', 'Pass_Attempted_Odds')) %>%
  pivot_wider(names_from = 'AttemptPassBoolean', values_from = odds) %>%
  mutate(odds_ratio = Pass_Attempted_Odds/No_Pass_Attempted_Odds,
         overall_odds = Total_Goals/Total_No_Goals,
         count = Total_Goals + Total_No_Goals) %>%
  select(Distance_Buckets, count, overall_odds, No_Pass_Attempted_Odds, Pass_Attempted_Odds, odds_ratio)


#Y Distance
Passing_Y_DistanceBuckets <- Entry_Information %>%
  mutate(Distance_Buckets = case_when(Y_Distance < 10 ~ '0-10',
                                      Y_Distance < 15 ~ '10-15',
                                      Y_Distance < 20 ~ '15-20',
                                      Y_Distance < 25 ~ '20-25',
                                      Y_Distance < 35 ~ '25-35',
                                      Y_Distance > 35 ~ '35-60'),
         Distance_Buckets = as.factor(Distance_Buckets),
         Distance_Buckets = factor(Distance_Buckets, levels = c('0-10', '10-15', '15-20', 
                                                                '20-25', '25-35', '35-60'))) %>%
  group_by(Distance_Buckets, AttemptPassBoolean, Goal) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(temp = paste(Distance_Buckets, AttemptPassBoolean),
         total = ave(count, temp, FUN=sum)) %>%
  select(-temp) %>%
  filter(Goal == 0) %>%
  mutate(Goal = total-count,
         NoGoal = count,
         odds = Goal/NoGoal,
         Total_Goals = ave(Goal, Distance_Buckets, FUN=sum),
         Total_No_Goals = ave(NoGoal, Distance_Buckets, FUN=sum)) %>%
  select(-c(count, total, Goal, NoGoal)) %>%
  mutate(AttemptPassBoolean = if_else(AttemptPassBoolean == 0, 'No_Pass_Attempted_Odds', 'Pass_Attempted_Odds')) %>%
  pivot_wider(names_from = 'AttemptPassBoolean', values_from = odds) %>%
  mutate(odds_ratio = Pass_Attempted_Odds/No_Pass_Attempted_Odds,
         overall_odds = Total_Goals/Total_No_Goals,
         count = Total_Goals + Total_No_Goals,
         Y_Distance_Buckets = Distance_Buckets) %>%
  select(Y_Distance_Buckets, count, overall_odds, No_Pass_Attempted_Odds, Pass_Attempted_Odds, odds_ratio)

#Graphs depicting what happened before 2on1----

#Failed Defensive Pass
prior_plays_graphing(Entry_Information, 'Failed Passes Defensive', T)
Event_Probability_Difference(Entry_Information, 'Failed Passes Defensive')

#LPR won by attacking team
prior_plays_graphing(Entry_Information, 'LPR Offensive Won', T)
Event_Probability_Difference(Entry_Information, 'LPR Offensive Won')

#Blocked Pass by Offensive team
prior_plays_graphing(Entry_Information, 'Pass Block Offensive', T)
Event_Probability_Difference(Entry_Information, 'Pass Block Offensive')

#Body check by offensive team
prior_plays_graphing(Entry_Information, 'Body Check Offensive', T)
Event_Probability_Difference(Entry_Information, 'Body Check Offensive')

#stick check by offensive team
prior_plays_graphing(Entry_Information, 'Stick Check Offensive', T)
Event_Probability_Difference(Entry_Information, 'Stick Check Offensive')

#failed stretch pass by defensive team
prior_plays_graphing(Entry_Information, 'Stretch Pass Fail Defensive', T)
Event_Probability_Difference(Entry_Information, 'Stretch Pass Fail Defensive')

#failed outlet pass by defensive team
prior_plays_graphing(Entry_Information, 'Outlet Pass Fail Defensive', T)
Event_Probability_Difference(Entry_Information, 'Outlet Pass Fail Defensive')

#Number of successful passes by offensive team
prior_plays_graphing(Entry_Information, 'Passes Success Offensive', T)
Event_Probability_Difference(Entry_Information, 'Passes Success Offensive')

#Number of successful stretches passes by offensive team
prior_plays_graphing(Entry_Information, 'Stretch Pass Success Offensive', T)
Event_Probability_Difference(Entry_Information, 'Stretch Pass Success Offensive')

#number of successful outlet passes by offensive team
prior_plays_graphing(Entry_Information, 'Outlet Pass Success Offensive', T)
Event_Probability_Difference(Entry_Information, 'Outlet Pass Success Offensive')


#counts of each preceeding event
Entry_Information %>%
  select(EntryID, `Failed Passes Defensive`:`Outlet Pass Success Offensive`) %>%
  pivot_longer(names_to = 'type', values_to = 'value', cols = `Failed Passes Defensive`:`Outlet Pass Success Offensive`) %>%
  filter(value != 0) %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(offset = case_when(count < 10 ~ 10,
                            count < 100 ~ 17.5,
                            TRUE ~ 25)) %>%
  ggplot(aes(x = reorder(type, count), y = count)) + geom_bar(stat = 'identity', fill = 'Dark Blue') + theme_bw() + 
    coord_flip() + ylim(0, 864) + geom_hline(yintercept = 864) + geom_text(aes(label = count, y = count + offset)) +
    labs(title = 'Count of Entries with Event Occuring 5 Seconds Before the 2on1 Entry', 
         x = 'Event Type', y = 'Number of Entries') + theme(plot.title = element_text(hjust=0.5))



# Looking closer at the impact of DefendingSameHandedness on pass and shooting success ----

#Shooting:
Entry_Information %>%
  filter(AttemptShotBoolean >= 1 & AttemptPassBoolean == 0) %>%
  mutate(SuccessfulShotBoolean = if_else(SuccessfulShots > 0, 1, 0)) %>%
  group_by(SuccessfulShotBoolean, DefendingSameHandedness) %>%
  summarise(count = n())

#Passing:
Entry_Information %>%
  filter(AttemptPassBoolean >= 1) %>%
  group_by(SuccessfulPassBoolean, DefendingSameHandedness) %>%
  summarise(count = n())


#Visualizing impact passing has on ability to shot----

Entry_Information %>%
  group_by(AttemptPassBoolean, AttemptShotBoolean) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = ave(count, AttemptPassBoolean, FUN=sum),
         percent = count/total,
         percent = round(100*percent, 2),
         percent = paste0(percent, '% \n '),
         percent = if_else(AttemptShotBoolean == 1, percent, NA_character_),
         AttemptPassBoolean = if_else(AttemptPassBoolean == 1, 'Yes', 'No'),
         AttemptShotBoolean = if_else(AttemptShotBoolean == 1, 'Yes', 'No'),
         AttemptShotBoolean = as.factor(AttemptShotBoolean),
         AttemptShotBoolean = factor(AttemptShotBoolean, levels = c('No', 'Yes'))) %>%
  ggplot(aes(x = AttemptPassBoolean, y = count, fill = AttemptShotBoolean, label = percent)) + theme_bw() +
  geom_bar(stat = 'identity', position = position_stack()) + geom_text(aes(y = total + 5)) +
  scale_fill_manual(values = c('#ADD8E6', '#01178B')) + ylim(c(0, 550)) +
  labs(x = 'Pass Attempted?', fill = 'Shot Attempted?', y = 'Number of Entries')

#Visualizing successful passes on the likelihood of scoring
Entry_Information %>%
  group_by(SuccessfulPassBoolean, Goal) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = ave(count, SuccessfulPassBoolean, FUN=sum),
         percent = count/total,
         percent = round(100*percent, 2),
         percent = paste0(percent, '% \n '),
         percent = if_else(Goal == 1, percent, NA_character_),
         SuccessfulPassBoolean = if_else(SuccessfulPassBoolean == 1, 'Yes', 'No'),
         Goal = if_else(Goal == 1, 'Yes', 'No'),
         Goal = as.factor(Goal),
         Goal = factor(Goal, levels = c('No', 'Yes'))) %>%
  ggplot(aes(x = SuccessfulPassBoolean, y = count, fill = Goal, label = percent)) + theme_bw() +
  geom_bar(stat = 'identity', position = position_stack()) + geom_text(aes(y = total + 5)) +
  scale_fill_manual(values = c('Light Blue', 'Dark Blue')) + ylim(c(0, 600)) +
  labs(x = 'Successful Pass?', fill = 'Goal?', y = 'Number of Entries')
  

#Visualizing attempted passes on the likelihood of scoring
Entry_Information %>%
  group_by(AttemptPassBoolean, Goal) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(total = ave(count, AttemptPassBoolean, FUN=sum),
         percent = count/total,
         percent = round(100*percent, 2),
         percent = paste0(percent, '% \n '),
         percent = if_else(Goal == 1, percent, NA_character_),
         AttemptPassBoolean = if_else(AttemptPassBoolean == 1, 'Yes', 'No'),
         Goal = if_else(Goal == 1, 'Yes', 'No'),
         Goal = as.factor(Goal),
         Goal = factor(Goal, levels = c('No', 'Yes'))) %>%
  ggplot(aes(x = AttemptPassBoolean, y = count, fill = Goal, label = percent)) + theme_bw() +
  geom_bar(stat = 'identity', position = position_stack()) + geom_text(aes(y = total + 5)) +
  scale_fill_manual(values = c('Light Blue', 'Dark Blue')) + ylim(c(0, 575)) +
  labs(x = 'Pass Attempted?', fill = 'Goal?', y = 'Number of Entries')



