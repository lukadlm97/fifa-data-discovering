data <- read.csv("data/data.csv")

str(data)

head(data[,4])
colnames(data[82])


lg_ds <- data[,4]
lg_ds <- data[,55:83]
lg_ds$Age <- data[,4]
lg_ds$Potential <- data[,'Potential']
head(lg_ds)
str(lg_ds)

library(corrplot)

corr.matrix <- cor(lg_ds)
corrplot.mixed(corr.matrix,tl.cex=0.75,number.cex=0.75)

cor(lg_ds[,2:4])

lm <- lm()

summary(lg_ds)

is.na(lg_ds[,5])
fix_ds <- NULL
iter <- 0
for (k in lg_ds[,1]){
  
  if(!is.na(k))
  {
    iter <- iter+1
   fix_ds$Crossing[iter] <- k 
  }
}

iter <- 0
for (k in lg_ds[,2]){
  if(!is.na(k))
  {
  iter <- iter+1
    fix_ds$Finisshing[iter] <- k 
  }
}

iter <- 0
for(j in lg_ds[,1:3]){
  for (k in lg_ds[,j]){
    if(!is.na(lg_ds[k,j]))
    {
      iter <- iter+1
      fix_ds[iter,colnames(lg_ds[,j])] <- k 
    }
  }
  iter <- 0
}

iter <- 0
for (k in lg_ds[,4]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$ShortPassing[iter] <- k 
  }
}

iter <- 0
for (k in lg_ds[,5]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Dribbling[iter] <- k 
  }
}

iter <- 0
for (k in lg_ds[,10]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Acceleration[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,11]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$SprintSpeed[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,16]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Jumping[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,17]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Stamin[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,20]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Aggression[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,'Age']){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Age[iter] <- k 
  }
}

iter <- 0
for (k in lg_ds[,21]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Overall[iter] <- k 
  }
}
iter <- 0
for (k in lg_ds[,2]){
  if(!is.na(k))
  {
    iter <- iter+1
    fix_ds$Potential[iter] <- k 
  }
}
summary(lg_ds)
str(lg_ds)

summary(fix_ds)  
fix_ds <- as.data.frame(fix_ds)
    
#clean data
str(fix_ds)
head(fix_ds)

corr.matrix <- cor(fix_ds)
corrplot.mixed(corr.matrix,lt.cex=0.75,number.cex=0.75)


fixed_lg <- fix_ds
fixed_lg$Overall <- NULL
corr.matrix <- cor(fixed_lg)
corrplot.mixed(corr.matrix,lt.cex=0.75,number.cex=0.75)
library(caret)

set.seed(120)
ind <- createDataPartition(fix_ds$Finisshing,p=0.8,list=FALSE)
test_data <- fixed_lg[-ind,]
train_data <- fixed_lg[ind,]

lm1 <- lm(Finisshing~Crossing+Dribbling+Acceleration+Aggression,train_data)
summary(lm1)

plot(lm1)
lm1.pred <- predict(lm1,test_data)

rss1 <- sum((lm1.pred-test_data$Finisshing)^2)
tss <- sum((mean(train_data$Finisshing)-test_data$Finisshing)^2)

rsq1 <- 1-(rss1/tss)
rsq1

rmse1 = sqrt(rss1/nrow(test_data))
rmse1

lm2 <- lm(Finisshing~.,train_data)
summary(lm2)

library(car)
vif(lm2)
sqrt(vif(lm2))
sort(sqrt(vif(lm2)))     

lm3 <- lm(Finisshing~.-Acceleration,train_data)
summary(lm3)

sort(sqrt(vif(lm3)))   

lm3.pred <- predict(lm3,test_data)

rss3 <- sum((lm3.pred-test_data$Finisshing)^2)
tss <- sum((mean(train_data$Finisshing)-test_data$Finisshing)^2)

rsq3 <- 1-(rss3/tss)
rsq3

rmse3 = sqrt(rss3/nrow(test_data))
rmse3

m3 <- c(rsq3,rmse3)
m3
m1 <- c(rsq1,rmse1)
a <- cbind(m1,m3)
a

str(data)

data$Position 

def_ds <- data[data$Position=='CB',]
def_ds
nrow(def_ds)

str(def_ds)
def_fix_ds <- def_ds[,55:83]
str(def_fix_ds)
def_fix_ds$Overall  <- def_ds[,'Overall']
summary(def_fix_ds)

def_cor <- cor(def_fix_ds)
corrplot.mixed(def_cor,lt.cex=0.75,number.cex=0.75)
str(def_fix_ds)

set.seed(120)
def_ind <- createDataPartition(def_fix_ds$Overall,p=0.8,list=FALSE)
def_test_data <- def_fix_ds[-def_ind,]
def_train_data <- def_fix_ds[def_ind,]

def_lm <- lm(Overall~HeadingAccuracy+ShortPassing+LongPassing+BallControl+Reactions+Aggression+Interceptions+Marking+Composure+StandingTackle+SlidingTackle,def_train_data)

summary(def_lm)

def_lm.pred <- predict(def_lm,def_test_data)

def_rss <- sum((def_lm.pred-def_test_data$Overall)^2)
def_tss <- sum((mean(def_train_data$Overall)-def_test_data$Overall)^2)

def_rsq <- 1-(def_rss/def_tss)
def_rsq

def_rmse = sqrt(def_rss/nrow(def_test_data))
def_rmse

m1 <- c(def_rsq,def_rmse)

def_all <- lm(Overall~.,def_train_data)
summary(def_all)

sort(sqrt(vif(def_all)))
def_bm <- lm(Overall~.-Balance-Agility-Penalties-StandingTackle-BallControl,def_train_data)
summary(def_bm)
sort(sqrt(vif(def_bm)))

def_bm.pred <- predict(def_bm,def_test_data)

def_rss1 <- sum((def_bm.pred-def_test_data$Overall)^2)
def_tss <- sum((mean(def_train_data$Overall)-def_test_data$Overall)^2)

def_rsq1 <- 1-(def_rss1/def_tss)
def_rsq1

def_rmse1 = sqrt(def_rss1/nrow(def_test_data))
def_rmse1

m2 <- c(def_rsq1,def_rmse1)

stats <- cbind(m1,m2)
stats
