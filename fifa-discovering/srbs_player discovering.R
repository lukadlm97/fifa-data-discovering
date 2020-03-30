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
  geom_histogram(bins = 10, 
                 fill = "red",
                 color = "blue")
