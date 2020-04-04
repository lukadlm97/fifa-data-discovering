ds <- read.csv("data/houses_to_rent_v2.csv")

summary(ds)

str(ds)

fixed_ds <- ds[,c(3,4,5,10,11,12,13)]

str(fixed_ds)

boxplot.stats(fixed_ds$rooms)$stats

apply(fixed_ds, 2, function(x)boxplot.stats(x)$out)

#rooms cleaning outliers
boxplot(fixed_ds$rooms)
boxplot(fixed_ds$rooms)$stat
quantile(fixed_ds$rooms,seq(0.9,1,0.025))
max_rooms <-quantile(fixed_ds$rooms,0.9) 
fixed_ds[fixed_ds$rooms>max_rooms,'rooms']<-max_rooms

#bathroom celaning outliers
boxplot(fixed_ds$bathroom)
boxplot(fixed_ds$bathroom)$stat
quantile(fixed_ds$bathroom,seq(0.9,1,0.025))
max_bathr <- quantile(fixed_ds$bathroom,0.975)
fixed_ds[fixed_ds$bathroom>max_bathr,'bathroom']<-max_bathr

#parking spaces celaning outliers
boxplot(fixed_ds$parking.spaces)
boxplot(fixed_ds$parking.spaces)$stat
quantile(fixed_ds$parking.spaces,seq(0.9,1,0.025))
max_parking <- quantile(fixed_ds$parking.spaces,0.95)
fixed_ds[fixed_ds$parking.spaces>max_parking,'parking.spaces']<-max_parking

#rent amount celaning outliers
boxplot(fixed_ds$rent.amount..R..)
boxplot(fixed_ds$rent.amount..R..)$stats
quantile(fixed_ds$rent.amount..R..,seq(0.9,1,0.025))
max_rent <- quantile(fixed_ds$rent.amount..R..,0.925)
fixed_ds[fixed_ds$rent.amount..R..>max_rent,'rent.amount..R..']<-max_rent


#rent total celaning outliers
boxplot(fixed_ds$total..R..)
boxplot(fixed_ds$total..R..)$stats
quantile(fixed_ds$total..R..,seq(0.9,1,0.025))
max_total <- quantile(fixed_ds$total..R..,0.925)
fixed_ds[fixed_ds$total..R..>max_total,'total..R..']<- max_total

str(fixed_ds)

data_set <- fixed_ds[,c(1,2,3,4,7)]
apply(data_set, 2, function(x)boxplot.stats(x)$stats)

#normalization
apply(data_set, 2, function(x)((x-min(x))/(max(x)-min(x))))

kmean <- kmeans(data_set,4,iter.max = 10,nstart = 1000)
kmean

eval_df <- data.frame()

source("Utility.R")

for(k in 2:8){
  km.res <- kmeans(data_set,k,iter.max=10,nstart=1000)
  eval_df <- rbind(eval_df,c(k,km.res$tot.withinss,km.res$betweenss/km.res$totss))
}


temp <-eval_df

names(eval_df) <- c("cluster","tot.within.ss","ratio")

library(ggplot2)

ggplot(eval_df,aes(x=cluster,y=tot.within.ss))+geom_line()

km4 <- kmeans(data_set,4,iter.max = 10,nstart=1000)
km4
