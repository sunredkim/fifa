# 2. 2019년도 발롱도르 순위와 선수 능력치의 상관관계
library(dplyr)
library(stringr)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()

url <- "https://en.wikipedia.org/wiki/2019_Ballon_d%27Or"
remDr$navigate(url)

library(XML)
elem <- remDr$findElement(using="css", value="#mw-content-text > div.mw-parser-output > table:nth-child(7)")
elemtxt <- elem$getElementAttribute("outerHTML")
elem_html <- htmlTreeParse(elemtxt, asText = TRUE, useInternalNodes = T, encoding="UTF-8")

Sys.setlocale("LC_ALL", "English") # 선수 이름 프랑스어 때문에 English로케일로
ballond19 <- readHTMLTable(elem_html, header = T, stringsAsFactors = FALSE)[[1]]
ballond19 <- ballond19 %>% rename("Rank"="Rank\n","Player"="Player\n","Club(s)"="Club(s)\n","Points"="Points\n")

##21행, 25행, 27행, 29행, 30행의 경우에는 공동 순위 때문에 한칸씩 밀려서 해결
naballond19 <- ballond19 %>% filter(is.na(Points)) %>% rename("Player"="Rank","Club(s)"="Player","Points"="Club(s)","Rank"="Points")

ballond19 <- ballond19 %>% subset(!(is.na(Points))) %>% bind_rows(naballond19)  %>% select(2:4) %>% mutate_at(vars(Points),as.numeric) %>% arrange(desc(Points))


