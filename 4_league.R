# 4. 리그 점수와 선수 등급과의 상관관계
library(dplyr)
library(stringr)
library(RSelenium)

remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4445, browserName = "chrome")
remDr$open()
url <- "https://www.uefa.com/memberassociations/uefarankings/country/#/yr/2021"
remDr$navigate(url)


library(XML)
elem <- remDr$findElement(using="css", value="#DataTables_Table_0 > tbody > tr")
elemtxt <- elem$getElementAttribute("outerHTML")
elem_html <- htmlTreeParse(elemtxt, asText = TRUE, useInternalNodes = T, encoding="UTF-8")

uefascore <- readHTMLTable(elem_html, header = T, stringsAsFactors = FALSE)[[1]]
