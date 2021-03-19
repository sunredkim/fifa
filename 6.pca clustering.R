View(fifa21)

library(dplyr)

positionplayer <- fifa21 %>% dplyr::select(team_position,overall,c(34:45),c(47:80))


# 특정 컬럼을 삭제 "SUB", "" "RES"

positionplayer$team_position
#  positionplayer <- positionplayer[!(positionplayer$team_position == "SUB")] (이 방법은 안됨)

position <- c("RF","CF","LF","ST","RS","LS","LW","RW","CDM","CM","CAM","RM","LM","LAM","RAM","LCM","RCM","LDM","RDM","RB","RWB","CB","LB","LWB","SW","LCB","RCB","GK")

newpositionplayer <- positionplayer %>% filter(team_position %in% position)


# 포지션을 크게 4가지로 재분류하여 저장 : case_when
newpositionplayer %>% 
  mutate(Newposition = case_when(team_position %in% c("RF","CF","LF","ST","RS","LS","LW","RW") ~ "FWD",
                                 team_position %in% c("CDM","CM","CAM","RM","LM","LAM","RAM","LCM","RCM","LDM","RDM") ~"MID",
                                 team_position %in% c("RB","RWB","CB","LB","LWB","SW","LCB","RCB")~ "DEF",
                                 team_position %in% c("GK")~"GK",
                                 TRUE ~ NA_character_)) ->newpositionplayer

View(newpositionplayer)
newpositionplayer <- newpositionplayer[-1]

table(newpositionplayer$Newposition) # 상위 500명을 대상으로 해야하는지

newpositionplayer[c(1:3),]

FWD <- newpositionplayer %>% filter(Newposition == "FWD") %>% arrange(desc(overall)) 
FWD <- FWD[c(1:500),]

MID <- newpositionplayer %>% filter(Newposition == "MID") %>% arrange(desc(overall))
MID <- MID[c(1:500),]

DEF <- newpositionplayer %>% filter(Newposition == "DEF") %>% arrange(desc(overall))
DEF <- DEF[c(1:500),]

GK <- newpositionplayer %>% filter(Newposition == "GK") %>% arrange(desc(overall))
GK <- GK[c(1:500),]



# PCA(골키퍼는 제외하자)
summary(FWD)
FWD <- FWD %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))

summary(MID)
MID <- MID %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))
View(MID)
summary(DEF)
DEF <- DEF %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))

summary(GK)
GK <- GK %>% dplyr::select(-c(pace,shooting,passing,dribbling,defending,physic,defending_marking))


best500 <- rbind(FWD,MID,DEF)
# 상관계수확인
cor(best500[8:40])

str(best500)
log.best500 <- log(best500[,8:40])
best500.position <- best500[,41]

best500.pca <- prcomp(log.best500,center = T,scale. = T)
print(best500.pca)
plot(best500.pca, type="l")


summary(best500.pca)
# pc22까지 해서 95%를 설명할 수 있으므로,PC22까지선택하도록 함.


PRC <- as.matrix(log.best500) %*% best500.pca$rotation
head(PRC)

install.packages("factoextra")
library(factoextra)


pca_fs <- prcomp(best500[,8:40], center=TRUE, scale=TRUE)

p3_fs <- fviz_pca_var(pca_fs, col.var = "Slate Blue", repel = TRUE)
p4_fs <- fviz_pca_ind(pca_fs, col.ind = "cos2", geom = "point", gradient.cols = c("yellow", "blue"))
grid.arrange(p3_fs, p4_fs, nrow=1)


View(FWD)
library(corrplot)

cor(FWD,use = "pairwise.complete.obs")
corrplot(cor(FWD,use = "pairwise.complete.obs"),method="color")

cor(MID,use = "pairwise.complete.obs")
cor(DEF,use = "pairwise.complete.obs")
cor(GK,use = "pairwise.complete.obs")