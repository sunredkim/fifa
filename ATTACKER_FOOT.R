library(car)

fifa <- read.csv('data/fifa21.csv')
fifa

Attacker<-read.csv('data/Attacker.csv')
Midfielder <- read.csv('data/Midfielder.csv')
Defender <- read.csv('data/Defender.csv')

rm(list = ls())
#
#주성분 분석

dim(Attacker)

# NA값 제거
sum(is.na(Attacker))
colSums(is.na(Attacker))
Attacker_D <- na.omit(Attacker)

View(Attacker)
# CHR 제거
Attacker_D <- Attacker_D[c(-1,-2,-3,-4)]
summary(Attacker_D)
str(Attacker_D)

#상관성 확인
Attack_cor<-cor(Attacker_D)
Attack_cor
#상관성 시각화
inAttackerall.packages('corrplot')
library(corrplot)

png('data/foot.png',height = 1200, width = 1500, pointsize = 15) #png 저장
par(mfrow=c(2,2))
plot(model)
dev.off()

corrplot.mixed(cor(Attacker_D),upper = 'shade') # 시각화
plot(Attacker_D)
pairs (Attack_cor,panel = panel.smooth)

#회귀분석

set.seed(12345)
idx <-createFolds(Attacker_D$foot)
test <-data.frame(Attacker_D[idx$Fold04,])
train <- data.frame(Attacker_D[-idx$Fold04,])

Attacker_D.lm<-lm(foot~.,data=train)
summary(Attacker_D.lm)

# 결과 값 비교
Attacker_D_pred <- predict(Attacker_D.lm,newdata = test )
Attacker_D_pred <- round(Attacker_D_pred)
result <- data.frame(Attacker_D_pred,test$foot)
prop.table(table(ifelse(result$Attacker_D_pred==result$test.foot,'0','X')))

#주성분 분석

PCA_Attacker<-prcomp(Attacker_D,center = T, scale. = T)
PCA_Attacker

#주성분 분석 시각화

screeplot(PCA_Attacker, npcs = 28, type = 'l')
summary(PCA_Attacker)

#주성분 회귀분석 실행

Attacker_3 <-as.matrix(Attacker_D) %*% PCA_Attacker$rotation[,1:7] #행렬곱 실시
Attacker_4 <- cbind(Attacker_3, as.data.frame(Attacker_D$foot))
colnames(Attacker_4)[12]<-'foot'
head(Attacker_4)

#다시 회귀 분석
library(caret)


set.seed(12345)
idx_pca <-createFolds(Attacker_4$foot)
test_pca <-data.frame(Attacker_4[idx$Fold04,])
train_pca <- data.frame(Attacker_4[-idx$Fold04,])

Attacker_pca <-lm(foot~.,data = train_pca)
summary(Attacker_pca)

#마지막 비교

Attacker_D_PCA_PRED <- predict(Attacker_pca,newdata = test_pca)
Attacker_D_PCA_PRED <- round(Attacker_D_PCA_PRED)
result_pca <- data.frame(Attacker_D_PCA_PRED,test_pca$foot)
prop.table(table(ifelse(result_pca$Attacker_D_PCA_PRED == result_pca$test_pca.foot,'x','o')))

