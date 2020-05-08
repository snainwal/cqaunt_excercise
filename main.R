---
title: "cquant_code"
author: "Shubham Nainwal"
date: "May 8, 2020"
output: pdf_document
---



library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)
library(gridExtra)




#####-------Task 1-------####




df_2016 <- read_csv("C:/Users/Shubham Nainwal/Desktop/cquant_excercise/historicalPriceData_ERCOT_DA_Prices_2016.csv")

df_2017 <- read_csv("C:/Users/Shubham Nainwal/Desktop/cquant_excercise/historicalPriceData_ERCOT_DA_Prices_2017.csv")

df_2018 <- read_csv("C:/Users/Shubham Nainwal/Desktop/cquant_excercise/historicalPriceData_ERCOT_DA_Prices_2018.csv")

df_2019 <- read_csv("C:/Users/Shubham Nainwal/Desktop/cquant_excercise/historicalPriceData_ERCOT_DA_Prices_2019.csv")




str(df_2016)
str(df_2017)
str(df_2018)
str(df_2019)




task_1 <- rbind(df_2016,df_2017,df_2018,df_2019)

str(df)

skim(df)




#####-------Task 2-------####



task_2 <- task_1



task_2$Year <- year(as.Date(task_2$Date))
task_2$Month <- months(as.Date(task_2$Date))

task_2 <- task_2[-1] 

task_2 <- task_2[,c(1,3,4,2)]



task_2 <- summarise(group_by(task_2,SettlementPoint,Year,Month), AvgPrice = mean(Price))

head(task_2, 5)




#####-------Task 3-------####




write.csv(task_2,"AveragePriceByMonth.csv", row.names = F)





#####-------Task 4-------####



task_4 <- task_1

task_4 <- separate(task_4, SettlementPoint, c("SettlementPoint", "Location"), sep="_")
task_4 <- filter(task_4, SettlementPoint =="HB", Price >0)
task_4$Year <- year(as.Date(task_4$Date))
task_4$Hour <- hour(as.POSIXct(task_4$Date))

task_4$SettlementPoint <- paste(task_4$SettlementPoint,task_4$Location, sep="_")
task_4 <- task_4[-c(1,3)]
task_4 <- arrange(task_4, SettlementPoint, Year, Hour)

task_4$change <-  log(lag(task_4$Price)) - log(task_4$Price)

task_4 <- summarise(group_by(task_4,SettlementPoint, Year), HourlyVolatility=sd(change))




#####-------Task 5-------####



write.csv(task_4,"HourlyVolatilityByYear.csv", row.names = F)




#####-------Task 6-------####



max_volatility <- summarise(group_by(task_4, Year), highest = max(HourlyVolatility, na.rm = TRUE))

task_6 <- filter(task_4, HourlyVolatility %in% max_volatility $highest)

write.csv(task_4,"MaxVolatilityByYear.csv", row.names = F)







#####-------Task 7-------####



task_7 <- task_1
task_7$Hour <- hour(as.POSIXct(task_7$Date))

task_7$Date <- format(as.POSIXct(task_7$Date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
task_7 <- task_7[,c(2,1,3,4)]

task_7 <- spread(task_7, Hour,Price)
 
location <- unique(task_7$SettlementPoint)

hb_busavg <- filter(task_7, SettlementPoint == location[1])
hb_houston <- filter(task_7, SettlementPoint == location[2])
hb_hubavg <- filter(task_7, SettlementPoint == location[3])
hb_north <- filter(task_7, SettlementPoint == location[4])
hb_south <- filter(task_7, SettlementPoint == location[5])
hb_west <- filter(task_7, SettlementPoint == location[6])
lz_aen <- filter(task_7, SettlementPoint == location[7])
lz_cps <- filter(task_7, SettlementPoint == location[8])
lz_houston <- filter(task_7, SettlementPoint == location[9])
lz_lcra <- filter(task_7, SettlementPoint == location[10])
lz_north <- filter(task_7, SettlementPoint == location[11])
lz_raybn <- filter(task_7, SettlementPoint == location[12])
lz_south <- filter(task_7, SettlementPoint == location[13])
lz_west <- filter(task_7, SettlementPoint == location[14])
hb_pan <- filter(task_7, SettlementPoint == location[15])



setwd("C:/Users/Shubham Nainwal/Desktop/cquant_excercise/FormattedSpotHistory")



write.csv(hb_busavg,"spot_hb_busavg.csv", row.names = F)
write.csv(hb_houston,"spot_hb_houston.csv", row.names = F)
write.csv(hb_hubavg,"spot_hb_hubavg.csv", row.names = F)
write.csv(hb_north,"spot_hb_north.csv", row.names = F)
write.csv(hb_south,"spot_hb_south.csv", row.names = F)
write.csv(hb_west,"spot_hb_west.csv", row.names = F)
write.csv(hb_pan,"spot_hb_pan.csv", row.names = F)
write.csv(lz_aen,"spot_lz_aen.csv", row.names = F)
write.csv(lz_cps,"spot_lz_cps.csv", row.names = F)
write.csv(lz_houston,"spot_lz_houston.csv", row.names = F)
write.csv(lz_lcra,"spot_lz_lcra.csv", row.names = F)
write.csv(lz_north,"spot_lz_north.csv", row.names = F)
write.csv(lz_south,"spot_lz_south.csv", row.names = F)
write.csv(lz_west,"spot_lz_west.csv", row.names = F)
write.csv(lz_raybn,"spot_lz_raybn.csv", row.names = F)




####-----Bonus-Mean Plot----####




Plot_1 <- separate(task_2, SettlementPoint, c("SettlementPoint", "Location"), sep="_")
Plot_1 <- filter(Plot_1, SettlementPoint =="HB")





