# https://www.kaggle.com/stefanoleone992/fifa-21-complete-player-dataset
Sys.setlocale("LC_ALL", "English")
fifa21 <- read.csv("data/players_21.csv",header=TRUE,encoding="UTF-8")
Sys.setlocale("LC_ALL","Korean")
head(fifa21)