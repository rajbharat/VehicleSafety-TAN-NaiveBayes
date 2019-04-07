library(VIM)
library(lattice)
library(mice)
library(ggplot2)


setwd("~/GITHUB/datascience-NUS/NaiveBayes/00 - Data")
#replace blank with NA
vehicle_safety = read.csv("vehicle_safety_NASS2010_2000_2012.csv", header=T, na.strings=c("","NA"))


str(vehicle_safety)
summary(vehicle_safety)
#remove vaalues whose target variable values are unknown 
vehicle_safety_Clean <- vehicle_safety[!(is.na(vehicle_safety$OA_MAIS) | vehicle_safety$OA_MAIS == ""), ]
summary(vehicle_safety_Clean)

data(vehicle_safety_Clean)
vehicle_safety_KNN <- kNN(vehicle_safety_Clean)
write.csv(vehicle_safety_KNN, file = "knn_impute_vehicle_safety_NASS2010_2000_2012.csv")


vehicle_safety_nummeric <- subset(vehicle_safety_Clean, select = -c(OA_BAGDEPLY,OA_SEX,OA_MANUSE,GV_WGTCDTR,VE_GAD1))
summary(vehicle_safety_Clean)
write.csv(vehicle_safety_Clean,"vehicle_safety_nummeric.csv")


md.pattern(vehicle_safety_nummeric)


mice_plot <- aggr(vehicle_safety_nummeric, col=c('yellow','red'),
                  numbers=TRUE, sortVars=TRUE, bars=TRUE, prop = TRUE,
                  labels=names(vehicle_safety_nummeric), cex.axis=.7,
                  gap=3, ylab=c("Missing data over columns","Overall Missing Plot"))

vehicle_safety_pmm <- mice(vehicle_safety_nummeric, m=2, maxit = 100, method = 'pmm', seed = 500)
vehicle_safety_pmm$imp$GV_CURBWGT

vehicle_safety_final <- complete(vehicle_safety_pmm,2)
summary(vehicle_safety_final)
write.csv(vehicle_safety_final,"vehicle_safety_final.csv")



