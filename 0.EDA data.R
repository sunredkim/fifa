# 어떤 데이터셋인지 확인하는 작업 (EDA)
# 은퇴선수, sub선수의 데이터는 제거함 
position <- c("RF","CF","LF","ST","RS","LS","LW","RW","CDM","CM","CAM","RM","LM","LAM","RAM","LCM","RCM","LDM","RDM","RB","RWB","CB","LB","LWB","SW","LCB","RCB","GK")

realfifa21 <- fifa21 %>% filter(team_position %in% position)

library(ggplot2)

ggplot(realfifa21) + geom_bar(aes(x = team_position), fill = "Blue") + ggtitle('FIFA 21 포지션별 선수들의 분포')


ggplot(realfifa21, aes(x=overall)) +    # 그래프를 그릴 데이터 지정
  geom_histogram(binwidth =0.5)        # 히스토그램 작성 
# overall 이 정규분포를 따르는지 확인?


ggplot(realfifa21,aes(x=overall, y= wage_eur ,color =team_position)) + geom_point(size=3) + ggtitle('FIFA 21 선수들') + theme(plot.title=element_text(size=24))




library(dplyr)
## MASS 패키지와 충돌하여 명시적으로 나타내어야 함. 
# team_position
fifaplayer <- realfifa21 %>% dplyr::select(attacking_crossing,attacking_finishing,attacking_heading_accuracy,attacking_short_passing, movement_acceleration,movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,mentality_composure,defending_standing_tackle,defending_sliding_tackle,goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,goalkeeping_reflexes)


playercor <- cor(fifaplayer)
library(corrplot)
corrplot(playercor)
corrplot(playercor, method="shade", type="lower", order="hclust", tl.srt=45, diag=F)
