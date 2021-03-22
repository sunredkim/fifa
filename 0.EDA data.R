library(showtext)

showtext_auto()
font_add(family = "nexonb", regular ="fonts/NEXONFootballGothicB.ttf")
font_add(family = "nexonl", regular ="fonts/NEXONFootballGothicL.ttf")

# 어떤 데이터셋인지 확인하는 작업 (EDA)
# 은퇴선수, sub선수의 데이터는 제거함 
position <- c("RF","CF","LF","ST","RS","LS","LW","RW","CDM","CM","CAM","RM","LM","LAM","RAM","LCM","RCM","LDM","RDM","RB","RWB","CB","LB","LWB","SW","LCB","RCB","GK")

realfifa21 <- fifa21 %>% filter(team_position %in% position)

library(ggplot2)

ggplot(realfifa21) + geom_bar(aes(x = team_position), fill = "Slate Blue") + ggtitle('FIFA 21 세부 포지션별 선수들의 분포') + xlab("포지션 명") + ylab("선수현황")+theme(plot.title = element_text(family = "nexonb", face = "bold", hjust = 0.5, size = 30, color = "darkblue"), axis.text.x = element_text(size = 10,family="nexonl"), axis.text.y = element_text(size = 12,family = "nexonl"),axis.title = element_text(family = "nexonl", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))



ggplot(newpositionplayer) + geom_bar(aes(x = Newposition), fill = "Slate Blue") + ggtitle('FIFA 21 포지션별 선수들의 분포') + xlab("포지션 명") + ylab("선수현황")+theme(plot.title = element_text(family = "nexonb", face = "bold", hjust = 0.5, size = 30, color = "darkblue"), axis.text.x = element_text(size = 10,family="nexonl"), axis.text.y = element_text(size = 12,family = "nexonl"),axis.title = element_text(family = "nexonl", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))





ggplot(realfifa21, aes(x=overall)) + geom_histogram(binwidth =0.5, fill = "Dark Slate Blue") + ggtitle('FIFA 21 오버롤의 분포') + xlab("오버롤") + ylab("선수현황")+theme(plot.title = element_text(family = "nexonb", face = "bold", hjust = 0.5, size = 30, color = "darkblue"), axis.text.x = element_text(size = 10,family="nexonl"), axis.text.y = element_text(size = 12,family = "nexonl"),axis.title = element_text(family = "nexonl", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))


# 히스토그램과 커널 밀도 함수 
ggplot(realfifa21, aes(x=overall, y=..density..)) + geom_histogram(binwidth =0.5, fill = "Dark Slate Blue", alpha=0.5) + geom_density(fill = NA, colour=NA, alpha=0.8) +  geom_line(stat="density") +   expand_limits(y=0)  + ggtitle('FIFA 21 오버롤의 분포') + xlab("오버롤") + ylab("선수현황")+theme(plot.title = element_text(family = "nexonb", face = "bold", hjust = 0.5, size = 30, color = "darkblue"), axis.text.x = element_text(size = 10,family="nexonl"), axis.text.y = element_text(size = 12,family = "nexonl"),axis.title = element_text(family = "nexonl", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))




ggplot(realfifa21,aes(x=overall, y= wage_eur ,color =team_position)) + geom_point(size=3) + ggtitle('FIFA 21 선수의 오버롤과 급여') + xlab("포지션 명") + ylab("선수현황") + theme(plot.title=element_text(size=24)) + theme(plot.title = element_text(family = "nexonb", face = "bold", hjust = 0.5, size = 30, color = "darkblue"), axis.text.x = element_text(size = 10,family="nexonl"), axis.text.y = element_text(size = 12,family = "nexonl"),axis.title = element_text(family = "nexonl", face = "bold", hjust = 0.5, size = 20, color = "darkblue"))




library(dplyr)
## MASS 패키지와 충돌하여 명시적으로 나타내어야 함. 
# team_position
fifaplayer <- realfifa21 %>% dplyr::select(attacking_crossing,attacking_finishing,attacking_heading_accuracy,attacking_short_passing, movement_acceleration,movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,mentality_composure,defending_standing_tackle,defending_sliding_tackle,goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,goalkeeping_reflexes)


playercor <- cor(fifaplayer)
library(corrplot)
corrplot(playercor)


corrplot(playercor, method="color", type="upper", order="hclust", tl.srt=45, diag=F, title = "Correlation Plot Of FIFA21 Data", tl.col = "Slate Blue", mar=c(0,0,1,0))




## 포지션별로 나누고, 각 포지션별로 능력치의 상관관계 분석



corrplot(cor(FWD %>% dplyr::select(c(-Newposition))), method="color", type="upper", order="hclust", tl.srt=45, diag=F, title = "Correlation Plot Of FWD's Data", tl.col = "Slate Blue", mar=c(0,0,1,0))



corrplot(cor(MID %>% dplyr::select(c(-Newposition))), method="color", type="upper", order="hclust", tl.srt=45, diag=F, title = "Correlation Plot Of MID's Data", tl.col = "Slate Blue", mar=c(0,0,1,0))



corrplot(cor(DEF %>% dplyr::select(c(-Newposition))), method="color", type="upper", order="hclust", tl.srt=45, diag=F, title = "Correlation Plot Of DEF's Data", tl.col = "Slate Blue", mar=c(0,0,1,0))



corrplot(cor(GK %>% dplyr::select(c(-Newposition))), method="color", type="upper", order="hclust", tl.srt=45, diag=F, title = "Correlation Plot Of GK's Data", tl.col = "Slate Blue", mar=c(0,0,1,0))




