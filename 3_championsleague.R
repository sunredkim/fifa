# 3. 챔피언스 리그 배당률과 게임에서의 급료와의 상관관계
library(dplyr)
library(stringr)
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
url <- "https://www.bet365.com/#/AC/B1/C172/D1/E51300756/F2" # 안 들어가짐. ?
remDr$navigate(url)
## 셀레늄 쓰지 말고 들어가보기



championsteam <-c("Manchester City", "FC Bayern München", "Paris Saint-Germain", "Liverpool", "Real Madrid", "Chelsea", "Borussia Dortmund", "Atlético Madrid", "FC Porto", "Lazio")

allwage <- fifa21  %>% group_by(club_name)%>% summarise(sumwage=mean(wage_eur))
View(allwage)