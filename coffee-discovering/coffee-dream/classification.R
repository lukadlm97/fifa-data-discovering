arabica_data_cleaned <- read.csv("data/arabica_data_cleaned.csv")


fix_num <- c(4,13,16,21,22,23:38,40)
fixed_ds <- arabica_data_cleaned[,fix_num]

str(fixed_ds)
ncol(fixed_ds)
fixed_ds<-fixed_ds[,-20]


summary(fixed_ds$Total.Cup.Points)

point.3q <- quantile(fixed_ds$Total.Cup.Points,0.75)

fixed_ds$HighPoint <- ifelse(test=fixed_ds$Total.Cup.Points>point.3q,yes='Yes',no='No')
fixed_ds$HighPoint <- as.factor(fixed_ds$HighPoint)

str(fixed_ds)

library(caret)
library(car)
library(rpart)
library(e1071)

new_ds <- fixed_ds
fixed_ds$Total.Cup.Points <- NULL

set.seed(120)
ind <- createDataPartition(fixed_ds$HighPoint,p=0.8,list=FALSE)
test_ds <- fixed_ds[-ind,]
train_ds <- fixed_ds[ind,]

folds <- trainControl(method='cv',number=10)
cpGrid <- expand.grid(.cp=seq(from=0.001,to=0.05,by=0.001))
set.seed(120)
best_cv <- train(HighPoint~.,
                 data=train_ds,
                 method='rpart',
                 control=rpart.control(minsplit=10),
                 trControl=folds,tuneGrid=cpGrid)
best_cv$bestTune$cp
any(is.na(train_ds))

summary(fixed_ds)
any(is.na(fixed_ds$Quakers))
mean.quaker <-  mean(fixed_ds$Quakers,na.rm=TRUE)
mean.quaker
fixed_ds[which(is.na(fixed_ds$Quakers)),'Quakers'] <-mean.quaker
fixed_ds[which(is.na(fixed_ds$Quakers)),]
fixed_ds[367,]<-NULL

summary(fixed_ds)

is.na(fixed_ds)
any(is.na(fixed_ds))

str(fixed_ds)
ncol(fixed_ds)
fixed_ds[,19]<-NULL


tree <- rpart(HighPoint~.,
              data=train_ds,
              method='class',
              control = rpart.control(minsplit = 10,cp=best_cv$bestTune$cp))

tree.pred <- predict(tree,test_ds,type='class')
tree.pred

tree.cm <- table(true=test_ds$HighPoint,predicted=tree.pred)
tree.cm

sum_evals <- rbind(eval_measure(tree.cm))


eval_measure <- function(cm){
  a <- sum(diag(cm))/sum(cm)
  p <- cm[1,1]/(cm[1,1]+cm[2,1])
  r <- cm[1,1]/(cm[1,1]+cm[1,2])
  f1 <- 2*p*r/(p+r)
  c(accurency=a,precision=p,recall=r,F1=f1)
  
}

sum_evals

# knn classification

knn_ds <- fixed_ds
knn_ds$Country.of.Origin <- NULL
knn_ds$Harvest.Year <- NULL
knn_ds$Color<-NULL
str(knn_ds)
ncol(knn_ds)

apply(knn_ds[,-16], 2, shapiro.test)
knn_test <- as.data.frame(apply(knn_ds[,-16], 2, function(x)scale(x,center = TRUE,scale = TRUE)))
head(knn_test)
knn_test$HighPoint <- knn_ds$HighPoint
str(knn_test)

set.seed(120)
index <- createDataPartition(knn_test$HighPoint,p=0.8,list=FALSE)
knn_test_ds <- knn_test[-ind,]
knn_train_ds <- knn_test[ind,]

folds <- trainControl(method='cv',number=10)
kGrid <- expand.grid(.k=seq(from=1,to=30,by=1))

set.seed(120)
cv_k <- train(HighPoint~.,
              data = knn_train_ds,
              method='knn',
              trControl=folds,tuneGrid=kGrid)
b_k <- cv_k$bestTune$k
b_k
library(class)
ncol(knn_train_ds)
knn <- knn(train=knn_train_ds[,-16],
           test=knn_test_ds[,-16],
           cl=knn_train_ds[,16],
           k=b_k)

knn.cm <- table(true=knn_test_ds$HighPoint,predicted=knn)
knn.cm

sum_evals <- rbind(sum_evals,eval_measure(knn.cm))
sum_evals

fixed_ds
str(fixed_ds)
ncol(fixed_ds)
num <-c(18,4:16,2) 
fixed_ds[,num]
str(fixed_ds[,num])

apply(fixed_ds[,num], 2, shapiro.test)

set.seed(120)
ind_nb <- createDataPartition(fixed_ds$HighPoint,p=0.8,list=FALSE)
nb_test <- fixed_ds[-ind_nb,]
nb_train <- fixed_ds[ind_nb,]

nb <- naiveBayes(HighPoint~.,
                 data=nb_train) 
nb.pred.prob <- predict(nb,newdata = nb_test,type='raw')

library(pROC)

roc_params <- roc(response=nb_test$HighPoint,
                  predictor = nb.pred.prob[,2])
roc_params$auc

plot.roc(roc_params,
         print.thres = TRUE,
         print.thres.best.method = 'youden')

roc_coords <- coords(roc_params,
                     ret = c('accuracy','spec','sens','thr'),
                     x='local maximas')
roc_coords

thresh <- roc_coords[9,4]
thresh

nb.pred.prob
nb.pred <- ifelse(test=nb.pred.prob[,1]>thresh,yes='Yes',no='No')
nb.pred
nb.pred<as.factor(nb.pred)

nb.cm <- table(true=nb_test$HighPoint,predicted=nb.pred)
nb.cm
