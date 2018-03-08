land <- read.csv("land2.csv")
summary(land)
#View(land)
head(land)

library("dplyr")

land$no_train_station_10km <- ifelse(is.na(land$no_train_station_10km), 0, land$no_train_station_10km)
land$no_hospital_2km <- ifelse(is.na(land$no_hospital_2km), 0, land$no_hospital_2km)
land$no_conv_store_2km <- ifelse(is.na(land$no_conv_store_2km), 0, land$no_conv_store_2km)
land$no_manufacturing_1km <- ifelse(is.na(land$no_manufacturing_1km), 0, land$no_manufacturing_1km)
land$no_police_2km <- ifelse(is.na(land$no_police_2km), 0, land$no_police_2km)
land$no_bus_station_500m <- ifelse(is.na(land$no_bus_station_500m), 0, land$no_bus_station_500m)
land$no_school_1km <- ifelse(is.na(land$no_school_1km), 0, land$no_school_1km)

land$min_dist_airport_10km <- ifelse(is.na(land$min_dist_airport_10km), 10000, land$min_dist_airport_10km)
land$min_dist_hospital_2km <- ifelse(is.na(land$min_dist_hospital_2km), 2000, land$min_dist_hospital_2km)
land$min_dist_school_2km <- ifelse(is.na(land$min_dist_school_2km), 2000, land$min_dist_school_2km)
land$dist_conv_store_2km <- ifelse(is.na(land$dist_conv_store_2km), 2000, land$dist_conv_store_2km)
land$dist_train_10km <- ifelse(is.na(land$dist_train_10km), 10000, land$dist_train_10km)
land$dist_min_police_2km <- ifelse(is.na(land$dist_min_police_2km), 2000, land$dist_min_police_2km)
land$dist_bus_500m <- ifelse(is.na(land$dist_bus_500m), 5000, land$dist_bus_500m)

land$min_dist_hospital_2km <- as.numeric(land$min_dist_hospital_2km)
land$min_dist_airport_10km <- as.numeric(land$min_dist_airport_10km)
land$min_dist_school_2km <- as.numeric(land$min_dist_school_2km)
land$dist_conv_store_2km <- as.numeric(land$dist_conv_store_2km)
land$dist_train_10km <- as.numeric(land$dist_train_10km)
land$dist_min_police_2km <- as.numeric(land$dist_min_police_2km)
land$dist_bus_500m <- as.numeric(land$dist_bus_500m)
#land$Area <- as.factor(land$Area)
land$BuildingType <- as.factor(land$BuildingType)

land <- land %>% dplyr::select(-BuiltUp, -SubArea, -Population, -Price, -Tenure, -Address)
data_land <- na.omit(land)
data_land$LandArea <- as.numeric(data_land$LandArea)
#data_land$Area <- as.numeric(data_land$Area)
data_land$Pricepsf <- as.numeric(data_land$Pricepsf)
data_land$CPI <- as.numeric(data_land$CPI)
str(data_land)
# Random Forest
library(randomForest)
rf <- randomForest(Pricepsf ~ ., data = data_land, mtry=1)

# tune random forest (mtry) manually
mse.rfs <- rep(0, 17)
for(m in 1:17){
  set.seed(12)
  rf <- randomForest(Pricepsf ~ ., data = data_land, mtry=m)
  mse.rfs[m] <- rf$mse[500]
}
plot(1:17, mse.rfs, type="b", xlab="mtry", ylab="OOB Error")


mtry.opt <- which.min(mse.rfs)

# fit a random forest model
set.seed(12)  # RF is random in both the bootstrap part and the predictor selection part
rf <- randomForest(Pricepsf ~ ., data = data_land, mtry = mtry.opt) 
data_land$PredictedPrice <- predict(rf, newdata = data_land)
View(data_land)

# Print predicted land prices for different cities
cities <- c("Ampang jaya", "Balakong", "Batu Arang", "Beranang", "Gombak", "Kajang", "Klang", 
            "Petaling Jaya", "Rawang", "Selayang", "Semenyih", "Serendah", "Shah Alam", "Subang Jaya")
land.price.hat <- data.frame(matrix(nrow = length(cities), ncol = 0))
land.price.hat <- cbind(cities, land.price.hat)
for (k in 1:length(cities)) {
  land.price.hat[k, "avgprice"] <- mean(data_land[data_land$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
land.price.hat

write.csv(data_land, file = "LandPrediction.csv")
