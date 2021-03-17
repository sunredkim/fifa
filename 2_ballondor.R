# 2. 2019년도 발롱도르 순위와 선수 능력치의 상관관계
# 프랑스어 선수명
library(dplyr)
library(stringr)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()

url <- "https://www.goal.com/en-za/lists/ballon-dor-2019-messi-ronaldo-van-dijk-set-to-head-up-list-of-30-/1ac0k3mdohx5y1rjinc2y98lv3"
remDr$navigate(url)
ballondordom <- remDr$findElements(using ="css selector","div.widget-slide-list__content > h2")
ballondor<-sapply(ballondordom,function(x){x$getElementText()})
is.list(ballondor)
ballondorlist <- data.frame(unlist(ballondor))

ballondorlist <- data.frame(do.call('rbind',strsplit(as.character(ballondorlist$unlist.ballondor.),split="|",fixed=TRUE)))

ballondorlist <- ballondorlist %>% rename(player=X1, team = X2, nation=X3)
ballondorlist$player <- str_sub(ballondorlist$player,start = 4) 
ballondorlist <- ballondorlist %>% mutate(ranking = c(1:30))
# fifa21 데이터에서 long_name 과 일치하는 것을 뽑아와야 함. 
View(fifa21)
View(ballondorlist)
ballondorname <- as.vector(unlist(ballondorlist %>% select(player)))
