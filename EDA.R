require(here)
require(magrittr)
require(dplyr)
require(ggplot2)


# Initial readin ----

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

Exp_and_Goals <- Events %>%
  group_by(gameid, teamid) %>%
  summarise(Goals = sum(name == 'goal'),
            Exp_Goals = sum(expectedgoals, na.rm = T))

#Goals
Events %>%
  filter(name == 'goal') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord)) + geom_point() + theme_classic()

#Shots
Events %>%
  filter(name == 'shot') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, color = playsection)) + geom_point() + theme_classic()

#Puck Protection
Events %>%
  filter(name == 'puckprotection') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord)) + geom_point() + theme_classic()


#Special Teams focus ----

Penalty_Kill <- Events %>%
  filter(skatersonicesituation == '4v5')

lpr_Penalty_Kill <- Penalty_Kill %>%
  filter(name == 'lpr') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, alpha = 0.33)) + geom_point() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

pass_block_Penalty_Kill <- Penalty_Kill %>%
  filter(name == 'block' & type == 'pass' & outcome == 'successful') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, alpha = 0.5)) + geom_point() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

goal_against_Penalty_Kill <- Penalty_Kill %>%
  filter(name == 'goalagainst') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, color = playsection)) + geom_point() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)


Power_Play <- Events %>%
  filter(skatersonicesituation == '5v4')

goal_Power_Play <- Power_Play %>%
  filter(name == 'goal') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, color = playsection)) + geom_point() + theme_classic() +
  geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

shot_Power_Play <- Power_Play %>%
  filter(name == 'shot') %>%
  ggplot(aes(x = xadjcoord, y = yadjcoord, color = playsection, alpha = 0.33)) + geom_point() + 
  theme_classic() + geom_vline(xintercept = 25, color = 'blue', size = 2) +
  geom_vline(xintercept = 0, color = 'red', size = 2) +
  geom_vline(xintercept = -25, color = 'blue', size = 2) + xlim(-100, 100) + ylim(-42.5, 42.5)

goals_shots_overlay_Power_Play %>%
  goal_against_Penalty_Kill


