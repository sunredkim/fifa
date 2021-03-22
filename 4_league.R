# 4. 리그 점수와 선수 등급과의 상관관계
library(dplyr)
library(stringr)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
url <- "https://www.uefa.com/memberassociations/uefarankings/country/#/yr/2021"
remDr$navigate(url)

score <- NULL
scores <- NULL
uefa <- NULL

# 리그 점수
scorenodes <- remDr$findElements(using='css selector', '#DataTables_Table_0 > tbody > tr > td.table_member-points')
score <- sapply(scorenodes, function(x) {x$getElementText()})
scores <- c(scores,unlist(score))

# 리그 이름
leaguenodes <- remDr$findElements(using='css selector', '#DataTables_Table_0 > tbody > tr > td.table_member-name > span > span.team-name.visible-md.visible-lg')

leagues <- sapply(leaguenodes, function(x) {x$getElementText()})
uefa <- c(uefa,unlist(leagues))

uefa <- as.list(uefa)
str(score)
uefaranking <- as.data.frame(cbind(uefa,score))
uefaranking
View(fifa21)

leaguenames <- c("English Premier League",
                 "Spain Primera Division",
                 "Ukrainian Premier League",
                 "German 1. Bundesliga",
                 "Italian Serie A",
                 "Czech Republic Gambrinus Liga",
                 "French Ligue 1",
                 "Russian Premier League",
                 "Greek Super League",
                 "Croatian Prva HNL",
                 "Portuguese Liga ZON SAGRES",
                 "Turkish Süper Lig",
                 "Belgian Jupiler Pro League",
                 "Holland Eredivisie",
                 "Austrian Football Bundesliga",
                 "Scottish Premiership",
                 "Danish Superliga",
                 "Romanian Liga I",
                 "Polish T-Mobile Ekstraklasa",
                 "Swiss Super League",
                 "Swedish Allsvenskan",
                 "Norwegian Eliteserien")

uefacountry <-c("Austria","Belgium","Croatia","Czech Republic","Denmark","England","France","Germany","Greece","Netherlands","Italy","Norway","Poland","Portugal","Romania","Russia","Scotland","Spain","Sweden","Switzerland","Turkey","Ukraine")

leagueoverall <-fifa21 %>% group_by(league_name) %>% summarise(mean_overall = mean(overall)) %>% filter(league_name %in% leaguenames) %>% cbind(uefacountry)
View(leagueoverall)

# uefa와 uefacountry 기준으로 조인 
uefacountry <- as.data.frame(unlist(uefaranking$uefa))
uefascore <- as.data.frame(as.numeric(unlist(uefaranking$score)))

uefaranking <- cbind(uefacountry,uefascore) 
names(uefaranking) = c("uefacountry","score")

uefarankingoverall <- inner_join(uefaranking,leagueoverall,by='uefacountry')

# 리그 점수와 선수 등급과의 상관관계
library(MASS)

plot(uefarankingoverall$mean_overall~uefarankingoverall$score,
     col="#1d3557", pch=19,
     xlab="UEFA score", ylab="player overall",
     main="UEFA League score and Overall of Players")


cor.test(uefarankingoverall$mean_overall,uefarankingoverall$score)

