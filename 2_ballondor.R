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
# 팀 이름 2개인 선수의 경우에 쉼표 표시 해줘도 될거 같은데 굳이 분석에는 필요하지 않을 것 같아 진행하지 않음

# 발롱도르 순위권에 해당하는 데이터들을 뽑아와야 함
print(ballond19)
View(fifa21)
View(ballond19)
name <- c("L. Messi",
          "V. van Dijk",
          "Cristiano Ronaldo",
          "S. Mané",
          "M. Salah",
          "K. Mbappé",
          "Alisson",
          "R. Lewandowski",
          "Bernardo Silva",
          "R. Mahrez",
          "F. de Jong",
          "R. Sterling",
          "E. Hazard",
          "K. De Bruyne",
          "M. de Ligt",
          "S. Agüero",
          "Roberto Firmino",
          "A. Griezmann",
          "T. Alexander-Arnold",
          "P. Aubameyang",
          "D. Tadic",
          "H. Son",
          "H. Lloris",
          "K. Koulibaly",
          "M. ter Stegen",
          "K. Benzema",
          "G. Wijnaldum",
          "João Félix",
          "Marquinhos",
          "D. van de Beek"
)


ballondplayer <- fifa21 %>% filter(short_name %in% name) %>% rbind(fifa21[197,])
View(ballondplayer)
str(ballondplayer)