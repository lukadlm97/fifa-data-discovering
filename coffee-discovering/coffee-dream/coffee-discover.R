library(readr)
arabica_data_cleaned <- read.csv("data/arabica_data_cleaned.csv")
str(arabica_data_cleaned)
?arabica_data_cleaned

summary(arabica_data_cleaned$Country.of.Origin)

summary(arabica_data_cleaned$Farm.Name)

summary(arabica_data_cleaned$Certification.Body)

fix_num <- c(4,13,16,21,22,23:38,40)

fixed_ds <- arabica_data_cleaned[,fix_num]

str(fixed_ds)
ncol(fixed_ds)
fixed_ds<-fixed_ds[,-20]

numerics <- c(2,4:17,19)
str(fixed_ds[,numerics])
lr_ds <- fixed_ds[,numerics]
valid <- c(2:12)
lr_ds <- lr_ds[,valid]
ncol(lr_ds)
library(corrplot)
cor.matrix <- cor(lr_ds)
corrplot.mixed(cor.matrix,lt.cex=0.75,number.cex=0.75)

str(lr_ds)

library(caret)
library(car)

set.seed(120)
ind  <- createDataPartition(lr_ds$Total.Cup.Points,p=0.8,list=FALSE)
test_data <- lr_ds[-ind,]
train_data <- lr_ds[ind,]

lm1 <- lm(Total.Cup.Points~.,train_data)
summary(lm1)

sort(sqrt(vif(lm1)))

lm2 <- lm(Total.Cup.Points~.-Aftertaste,train_data)
summary(lm2)

lm2.pred <- predict(lm2,test_data)

rss <- sum((lm2.pred-test_data$Total.Cup.Points)^2)
tss <- sum((mean(train_data$Total.Cup.Points)-test_data$Total.Cup.Points)^2)

rsq <- 1-rss/tss
rsq
rmse <- sqrt(rss/nrow(test_data))
rmse

lm1.pred <- predict(lm1,train_data)

rss1 <- sum((lm1.pred-test_data$Total.Cup.Points)^2)
tss <- sum((mean(train_data$Total.Cup.Points)-test_data$Total.Cup.Points)^2)

rsq1 <- 1-rss1/tss
rsq1
rmse1 <- sqrt(rss1/nrow(test_data))
rmse1
