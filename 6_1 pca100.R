View(fifa21)

library(dplyr)
install.packages("psych")

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

table(newpositionplayer$Newposition) # 상위 500명을 대상으로 

newpositionplayer[c(1:3),]

FWD <- newpositionplayer %>% filter(Newposition == "FWD") %>% arrange(desc(overall)) 
FWD100 <- FWD[c(1:100),]

MID <- newpositionplayer %>% filter(Newposition == "MID") %>% arrange(desc(overall))
MID100 <- MID[c(1:100),]

DEF <- newpositionplayer %>% filter(Newposition == "DEF") %>% arrange(desc(overall))
DEF100 <- DEF[c(1:500),]

GK <- newpositionplayer %>% filter(Newposition == "GK") %>% arrange(desc(overall))
GK100 <- GK[c(1:100),]



# PCA(골키퍼는 제외하자)
FWD100 <- FWD100 %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))

MID100 <- MID100 %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))
DEF100 <- DEF100 %>% dplyr::select(-c(gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,defending_marking))

GK100 <- GK100 %>% dplyr::select(-c(pace,shooting,passing,dribbling,defending,physic,defending_marking))

best100 <- rbind(FWD100,MID100,DEF100)


# 상관계수확인
cor(best100[8:40])

str(best100)
log.best100 <- log(best100[,8:40])
best100.position <- best100[,41]

best100.pca <- prcomp(log.best100,center = T,scale. = T)
print(best100.pca)
summary(best100.pca)
# summary 결과의 cumulative proportion 비율이 90% 까지 선택하는 것이 일반적이다. 15

plot(best100.pca, type="l")

# 그래프가 완만해지는 부분 이전까지만 사용하는 것이 바람직함
screeplot(best100.pca, main = "", col = "purple", type = "lines", pch = 1, npcs = length(best100.pca$sdev))

biplot(best100.pca)
nyv <- predict(best100.pca)[, 1]
nyv2 <- predict(best100.pca)[, 2]
par(mfrow = c(1,2))
plot(best100$defending, nyv, pch = 16, xlab = "defending", ylab = "PC 1", col = "red")
plot(best100$shooting, nyv2, pch = 16, xlab = "shooting", ylab = "PC 2", col = "blue")


nPRC <- as.matrix(log.best100) %*% best100.pca$rotation
head(nPRC)

library(factoextra)

npca_fs <- prcomp(best100[,8:40], center=TRUE, scale=TRUE)

np3_fs <- fviz_pca_var(npca_fs, col.var = "Slate Blue", repel = TRUE)
np4_fs <- fviz_pca_ind(npca_fs, col.ind = "cos2", geom = "point", gradient.cols = c("yellow", "blue"))


fviz_pca_biplot(npca_fs, repel = FALSE)
fviz_pca_var(npca_fs, col.var="contrib",
             gradient.cols = "Set2",
             repel = TRUE # Avoid text overlapping
)

# 어떤 요인을 주요인으로 할 것인지
# 몇 개를 뽑을 것인지 결정은 best500.pca 그래프에서 고유근이 1이상인 경우까지 인자수를 채택하곤 해서 7까지만 선택하겠다는 판단을 했다. & pca plot 

library(psych)
# 그래프 상에서

np5_os <- principal(best100[,8:40], nfactors=7, rotate="varimax")
print(loadings(np5_os), digits=2, cutoff=0.4, sort=TRUE)


# 누적확률 90% 15
np6_os <- principal(best100[,8:40], nfactors=15, rotate="varimax")
print(loadings(np6_os), digits=3, cutoff=0.4, sort=TRUE)





library(corrplot)

corrplot(cor(FWD100 %>% dplyr::select(c(-Newposition)),use = "pairwise.complete.obs"),method="color")

corrplot(cor(DEF100 %>% dplyr::select(c(-Newposition)),use = "pairwise.complete.obs"),method="color")
corrplot(cor(MID100 %>% dplyr::select(c(-Newposition)),use = "pairwise.complete.obs"),method="color")
corrplot(cor(GK100 %>% dplyr::select(c(-Newposition)),use = "pairwise.complete.obs"),method="color")
