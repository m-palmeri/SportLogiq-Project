require(here)
require(dplyr)
require(magrittr)



#Data readin  and functions----

Games <- read.csv(here('Data', 'game_info.csv'))
Players <- read.csv(here('Data', 'player_info.csv'))
Teams <- read.csv(here('Data', 'team_info.csv'))
Events <- read.csv(here('Data', 'compiled_events.csv'))


#adding pass_id

pass_id = 0
current_gameid = 0
n = nrow(Events)
passid = numeric(n)
for(i in (1:n)) {
  temp <- Events[i,]
  if (temp$gameid != currentgamid) {
    current_gameid = temp$gameid
    pass_id = pass_id + 1
  }
  if (temp$name == 'pass') {
    
  }
  
}




