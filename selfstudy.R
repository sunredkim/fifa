library(dplyr)
## MASS 패키지와 충돌하여 명시적으로 나타내어야 함. 
# team_position
fifaplayer <- fifa21 %>% dplyr::select(attacking_crossing,attacking_finishing,attacking_heading_accuracy,attacking_short_passing, movement_acceleration,movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,mentality_composure,defending_standing_tackle,defending_sliding_tackle,goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,goalkeeping_reflexes)

# 팀 포지션의 종류 
distinct(fifaplayer$team_position)

playercor <- cor(fifaplayer)
library(corrplot)
corrplot(playercor)
corrplot(playercor, method="shade", type="lower", order="hclust", tl.srt=45, diag=F)