data <- read.csv("data/data.csv")

str(data)

head(data$Nationality,40)
data[data$Nationality=='Serbia',]
srb_players <- data[data$Nationality=='Serbia',]
summary(srb_players)

srb_players$Name

summary(srb_players$Position)

cm_srb = srb_players[srb_players$Position == 'CM',]
cm_srb$Name
amc_srb = srb_players[srb_players$Position == 'CAM',]
amc_srb$Name
dm_srb = srb_players[srb_players$Position == 'CDM',]
dm_srb$Name

mid_srb <- rbind(dm_srb,amc_srb,cm_srb)
mid_srb$Name

srb_players
srb <- srb_players[,55:83]
str(srb)
srb$Age <- srb_players$Age
srb$Overall <- srb_players$Overall
srb$Positioning <- srb_players$Position
srb$Height <- srb_players$Height
srb$Weight <- srb_players$Weight

library(caret)
library(ggplot2)
library(car)
summary(srb)

table(srb$Positioning)

table(srb$Positioning,srb$Age)

plot(density(srb$Age))
plot(density(srb$Overall))

ggplot(data = srb, 
       mapping = aes(x = 'Age')) +
  geom_density(alpha=20)

str(srb)
srb[,23]

num <- c(1:22,24:31)

str(srb[,num])
cor.matrix <- cor(srb[,num])
library(corrplot)
corrplot.mixed(cor.matrix,lt.cex=0.75,number.cex=0.75)

set.seed(120)
ind <- createDataPartition(srb[,num]$Overall,p=0.8,list=FALSE)
test_data <- srb[-ind,num]
train_data <- srb[ind,num]

lm1 <- lm(Overall~ShortPassing+FKAccuracy+LongPassing+BallControl+Reactions+Composure,train_data)
summary(lm1)

lm1.pred <- predict(lm1,test_data)
rss1 <- sum((lm1.pred-test_data$Overall)^2)
tss <- sum((mean(train_data$Overall)-test_data$Overall)^2)
rsq1 <- 1-rss1/tss
rsq1

rmse1 <- sqrt(rss1/nrow(test_data))
rmse1

lm2 <- lm(Overall~.,train_data)
summary(lm2)

sort(sqrt(vif(lm2)))

lm3 <- lm(Overall~.-FKAccuracy-Vision-StandingTackle-Dribbling-LongShots-Marking,train_data)
summary(lm3)

sort(sqrt(vif(lm3)))

lm3.pred <- predict(lm3,train_data)
rss3 <- sum((lm3.pred-test_data$Overall)^2)
tss <- sum((mean(train_data$Overall)-test_data$Overall)^2)
rsq3 <- 1-rss3/tss
rsq3

rmse3 <- sqrt(rss3/nrow(test_data))
rmse3

library(rpart)

srb_players
str(srb)

pl.q <- quantile(srb$Overall,0.6)
pl.q

ct_srb <- srb
ct_srb$HighQuality <- ifelse(test=ct_srb$Overall>pl.q,yes='Yes',no='No')
ct_srb$HighQuality <- as.factor(ct_srb$HighQuality)
ct_srb$Overall <- NULL
str(ct_srb)
table(ct_srb$HighQuality)

set.seed(1200)
ind <- createDataPartition(ct_srb$HighQuality,p=0.8,list=FALSE)
tr_data <- ct_srb[ind,]
ts_data <- ct_srb[-ind,]

library(e1071)
folds <- trainControl(method='cv',number=10)
cpGrid <- expand.grid(.cp=seq(from=0.001,to=0.05,by=0.001))
set.seed(120)
best_cv <- train(HighQuality~.,
                 data=tr_data,
                 method='rpart',
                 control=rpart.control(minsplit = 10),
                 trControl=folds,tuneGrid=cpGrid)
cp_o <- best_cv$bestTune$cp
cp_o

library(rpart)
ct <- rpart(HighQuality~.,
                  data=tr_data,
                  method='class',
                  control=rpart.control(minsplit=10,cp=cp_o))
ct.pruned <- prune(ct.model,cp=cp)

ct.pred <- predict(ct.model,ts_data,type='class')
ct.pred

ct.cm <- table(true=ts_data$HighQuality,predicted=ct.pred)
ct.cm

eval_measure <- function(cm){
  a <- sum(diag(cm))/sum(cm)
  p <- cm[1,1]/(cm[1,1]+cm[2,1])
  r <- cm[1,1]/(cm[1,1]+cm[1,2])
  f1<- 2*r*p/(r+p)
  c(accurency = a,precision=p,recall=r,F1=f1)
}
eval_measure(ct.cm)

knn_ds <- srb
str(knn_ds)

knn_ds$HighQuality <- ifelse(test=knn_ds$Overall>pl.q,yes='Yes',no='No')
knn_ds$Overall<-NULL
knn_ds$Positioning <- NULL
knn_ds$HighQuality <- as.factor(knn_ds$HighQuality)
knn_ds$Height <- as.numeric(knn_ds$Height)
knn_ds$Weight <- as.numeric(knn_ds$Weight)

apply(knn_ds[,-32],2,shapiro.test)

knn_ds$Agility <- scale(knn_ds$Agility,center=mean(knn_ds$Agility),scale = IQR(knn_ds$Agility))
knn_ds$Reactions <- scale(knn_ds$Reactions,center = mean(knn_ds$Reactions,scale=IQR(knn_ds$Reactions))

set.seed(120)
ind <- createDataPartition(knn_ds$HighQuality,p=0.8,list=FALSE)
test_ds <- knn_ds[-ind,]
train_ds <- knn_ds[ind,]

k_folds <- trainControl(method='cv',number=10)
kGrid <- expand.grid(.k=seq(from=1,to=30,by=1))

best_k <- train(HighQuality~.,
                train_ds,
                method='knn',
                trControl=k_folds,tuneGrid=kGrid)
k_opt <- best_k$bestTune$k
k_opt
library(class)
ncol(train_ds[,-32])
train_ds[,32]
train_ds[,-32]
train_ds[,32]
knn.model = knn(train_ds[,-32],
                test_ds[,-32],
                cl=train_ds[,32],
                k=k_opt)

knn.cm <- table(true=test_ds$HighQuality,predicted=knn.model)
knn.cm

eval_measure(knn.cm)

nb_ds <- srb
nb_ds$HighQuality <- ifelse(test=nb_ds$Overall>pl.q,yes='Yes',no='No')
nb_ds$HighQuality <- as.factor(nb_ds$HighQuality)

library(bnlearn)
nb_ds$Agility
str(nb_ds)
nb_ds$Agility
nb_ds$Agility = discretize(nb_ds[,],method='quantile',breaks = c(5))
