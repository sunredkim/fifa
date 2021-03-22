# 선수의 세부능력치를 통해 overall 예측

View(fifa21)

library(dplyr)
install.packages("psych")
install.packages("nFactors")

library(nFactors)
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
View(MID)

best500 <- rbind(FWD,MID,DEF)


# 상관계수확인
cor(best500[8:40])
View(best500)
str(best500)
plot(best500[,8:40])
log.best500 <- log(best500[,8:40])
best500.position <- best500[,41]

best500.pca <- prcomp(log.best500,center = T,scale. = T)
print(best500.pca)
summary(best500.pca)
# summary 결과의 cumulative proportion 비율이 90% 

plot(best500.pca, type="l")

# 그래프가 완만해지는 부분 이전까지만 사용하는 것이 바람직함 (7),, pca 분석에서 누적 비율이 70%~ 90% 인 것을 기준으로 결정한다고 해서. 7개의 주성분으로 결정함 (1) 누적기여율(설명된 분산의 누적 비율)이 최소 (at least) 0.8 이상일 것.

#(2) 단지 평균 분산보다 큰 PC만 선별할 것.

#(만약 표준화한 데이터에 대한 상관관계행렬을 사용할 경우 고유값(eigenvalue)이 최소 1보다 큰 PC)

#(3) Scree plot 을 그려봤을 때 꺽이는 부분 (elbow)이 있다면 elbow 지점 앞의 PC 개수 선택.


screeplot(best500.pca, main = "", col = "purple", type = "lines", pch = 1, npcs = length(best500.pca$sdev))

biplot(best500.pca)
yv <- predict(best500.pca)[, 1]
yv2 <- predict(best500.pca)[, 2]
par(mfrow = c(1,2))
plot(best500$defending, yv, pch = 16, xlab = "defending", ylab = "PC 1", col = "red")
plot(best500$shooting, yv2, pch = 16, xlab = "shooting", ylab = "PC 2", col = "blue")


PRC <- as.matrix(log.best500) %*% best500.pca$rotation
head(PRC)


e_value <- eigen(cor(best500[,8:40]))
install.packages("factoextra")
library(factoextra)

pca_fs <- prcomp(best500[,8:40], center=TRUE, scale=TRUE)

p3_fs <- fviz_pca_var(pca_fs, col.var = "Slate Blue", repel = TRUE)
p4_fs <- fviz_pca_ind(pca_fs, col.ind = "cos2", geom = "point", gradient.cols = c("yellow", "blue"))


fviz_pca_biplot(pca_fs, repel = FALSE)
fviz_pca_var(pca_fs, col.var="contrib",
             gradient.cols = "Set1",
             repel = TRUE # Avoid text overlapping
)

# 어떤 요인을 주요인으로 할 것인지
# 몇 개를 뽑을 것인지 결정은 best500.pca 그래프에서 고유근이 1이상인 경우까지 인자수를 채택하곤 해서 7까지만 선택하겠다는 판단을 했다. & pca plot 

library(psych)
# 그래프 상에서주성분 분석 70%, 7개. 

p5_os <- principal(best500[,8:40], nfactors=7, rotate="varimax")
print(loadings(p5_os), digits=2, cutoff=0.4, sort=TRUE)



new_reg<-lm(best500$overall~pca_fs$x[,1]+pca_fs$x[,2]+pca_fs$x[,3]+pca_fs$x[,4]+pca_fs$x[,5]+pca_fs$x[,6]+pca_fs$x[,7])

summary(new_reg)


#주성분회귀분석과 비교를 위해서 stepwise 방식의 회귀모델을 분석한다.
model_step<-lm(best500$overall~.,data=best500[,8:40])
#독립변수를 하나씩 제거해가면서 AIC가 최소가 되는 모델을 찾아본다.
model_step<-step(model_step,direction = "backward", trace=T)
summary(model_step)

nx=data.frame(data=best500[,8:40])
pre=predict(new_reg,nx,interval="prediction")
pre

#비교의 방법은 fit(예측값)과 실제값의 부호가 같은 경우 정답이라고 인식
#그 다음으로 비교하는 방법은 실제값이 lwr, upr 사이에 들어오는지 비교 
testset<-cbind(pre,best500$overall)
testset
View(testset)

View(fifa21 %>% dplyr::select(c(overall,short_name)))
