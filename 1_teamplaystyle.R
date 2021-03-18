# 1. 수비력이 좋은 팀 공격력이 좋은 팀을 찾아보고 20-21 시즌의 실제 득점 실점 과의 관계 프리미어 리그 
# https://sports.daum.net/record/epl/team?season=20192020

library(dplyr)
library(stringr)
View(epl2020)
epl2020 <- read.csv("data/epl2020.csv",header=TRUE,encoding="UTF-8")

Liverpool <- epl2020 %>% filter(teamId == "Liverpool")
View(Chelsea)
Norwich <- epl2020 %>% filter(teamId == "Norwich")
ManCity <- epl2020 %>% filter(teamId == "Man City")
WestHam<- epl2020 %>% filter(teamId == "West Ham")
Bournemouth <- epl2020 %>% filter(teamId == "Bournemouth")
Brighton <- epl2020 %>% filter(teamId == "Brighton")
Burnley <- epl2020 %>% filter(teamId == "Burnley")
CrystalPalace <- epl2020 %>% filter(teamId == "Crystal Palace")
Everton <- epl2020 %>% filter(teamId == "Everton")
SheffieldUnited <- epl2020 %>% filter(teamId == "Sheffield United")
Southampton <- epl2020 %>% filter(teamId == "Southampton")
Watford <- epl2020 %>% filter(teamId == "Watford")
AstonVilla <- epl2020 %>% filter(teamId == "Aston Villa")
Tottenham <- epl2020 %>% filter(teamId == "Tottenham")
Arsenal <- epl2020 %>% filter(teamId == "Arsenal")
Leicester <- epl2020 %>% filter(teamId == "Leicester")
NewcastleUnited <- epl2020 %>% filter(teamId == "Newcastle United")
Wolves <- epl2020 %>% filter(teamId == "Wolves")
Chelsea <- epl2020 %>% filter(teamId == "Chelsea")
ManUtd <- epl2020 %>% filter(teamId == "Man Utd")

mean(Chelsea$xG)
mean(Liverpool$xG)
mean(ManUtd$xG)

# xG : 기대득점
# xGA : 기대 실점
# xGD : 기대 득실차 