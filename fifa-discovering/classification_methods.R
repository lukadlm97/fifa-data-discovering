data <- read.csv("data/data.csv")
?data

str(data)
data$Position


st_data <- data[data$Position == 'ST',]
nrow(st_data)
summary(st_data)

class_data <- st_data[,55:83]
class_data
str(class_data)
class_data$Age <- st_data[,'Age']

library(car)
library(caret)

corr.matrix <- cor(class_data)
corrplot.mixed(corr.matrix,lt.cex=0.75,number.cex=0.75)
corr.matrix
num <- c(3,4,5,6,10,14,16,20,23,24,25,26)
st_ds <- class_data[,num]
colnames(st_ds)
st_ds$Finishing <- class_data$Finishing
str(st_ds)

finish.3q <- quantile(st_ds$Finishing,0.75)
summary(st_ds$Finishing)
finish.3q

st_ds$Scorer <- ifelse(test=st_ds$Finishing>finish.3q,yes = 'Yes',no='No')
st_ds$Scorer <- as.factor(st_ds$Scorer)
str(st_ds)
summary(st_ds$Scorer)

library(rpart)

st_ds$Finishing <-NULL 

set.seed(120)
ind <- createDataPartition(st_ds$Scorer,p=0.8,list=FALSE)
test_data <- st_ds[-ind,]
train_data <- st_ds[ind,]

library(e1071)

folds = trainControl(method='cv',number=10)
cpGrid = expand.grid(.cp=seq(from=0.001,by=0.001,to=0.05))
set.seed(120)
cv_best <- train(Scorer~.,
                 data=train_data,
                 method='rpart',
                 control=rpart.control(minsplit=10),
                 trControl = folds,tuneGrid=cpGrid)

best_cp <- cv_best$bestTune$cp
best_cp

tree <- rpart(Scorer~.,
              data=train_data,
              method='class')

tree.pred <- predict(tree,test_data,type='class')
head(tree.pred)

tree.cm <- table(true=test_data$Scorer,predicted=tree.pred)
tree.cm

eval_measure <- function(cm){
  TP <- cm[2,2]
  TN <- cm[1,1]
  FP <- cm[2,1]
  FN <- cm[1,2]
  a <- sum(TP+TN)/sum(cm)
  p <- TP/(TP+FP)
  r <- TP/(TP+FN)
  f1 <- 2*p*r/(p+r)
  c(accurency=a,precison=p,recall=r,F1=f1)
}

ct.em <- eval_measure(tree.cm)
ct.em
library(class)

folds_k <- trainControl(method='cv',number = 10)
kGrid <- expand.grid(.k=seq(from=1,to=30,by=1))
set.seed(120)
cv_k_best <- train(Scorer~.,
                   data=train_data,
                   method='knn',
                   trControl=folds_k,tuneGrid=kGrid)

k_koef <- cv_k_best$bestTune$k
k_koef
str(train_data[,13])
knn.pred <- knn(train=train_data[,-13],
                 test=test_data[,-13],
                 cl=train_data[,13],
                 k=k_koef)

knn.cm <- table(true=knn.pred,predicted=test_data$Scorer)
knn.cm
tree.cm

knn.em <- eval_measure(knn.cm)
knn.em

sum_stat <- cbind(ct.em,knn.em)
sum_stat

