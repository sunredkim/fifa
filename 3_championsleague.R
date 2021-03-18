# 3. 챔피언스 리그 배당률과 게임에서의 급료와의 상관관계
library(dplyr)
library(stringr)
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
url <- "https://www.bet365.com/#/AC/B1/C172/D1/E51300756/F2" # 안 들어가짐
remDr$navigate(url)

# 웹 크롤링에 필요한 라이브러리 로드
library(rvest)
library(XML)
library(httr)

web=GET("https://www.bet365.com/#/AC/B1/C172/D1/E51300756/F2")
web = htmlParse(web)
betsite

xpath1 = '/html/body/div[1]/div/div[2]/div[3]/div/div/div/div[1]/div/div/div[2]/div/div/div[2]/div/div/div/span[2]'

xpath2 =  '/html/body/div[1]/div/div[2]/div[3]/div/div/div/div[1]/div/div/div[2]/div/div/div[2]/div/div/div/span[1]'

teamnames <- xpathSApply(web,xpath1,xmlValue)

url <- "https://www.bet365.com/#/AC/B1/C172/D1/E51300756/F2"
betsite <-read_html(url)
teamnames <-web %>% html_nodes("span.gl-ParticipantBorderless_Name") %>% html_text()

betsite = read_html('https://www.bet365.com/#/AC/B1/C172/D1/E51300756/F2')
teamname = html_nodes(betsite,'body > div:nth-child(1) > div > div.wc-WebConsoleModule_SiteContainer > div.wc-PageView > div > div > div > div.wcl-PageContainer_Colcontainer > div > div > div.cm-CouponModule > div > div > div.gl-MarketGroup_Wrapper.src-MarketGroup_Container > div > div > div')
teamnames = html_text(teamname)
teamnames


# 8강 진출팀의 평균 급료 추출
championsteam <-c("Manchester City", "FC Bayern München", "Paris Saint-Germain", "Liverpool", "Real Madrid", "Chelsea", "Borussia Dortmund", "FC Porto")

allwage <- fifa21  %>% group_by(club_name)%>% summarise(sumwage=mean(wage_eur)) %>% filter(club_name %in% championsteam)