library(tidyr)
library(dplyr)
library(xlsx)
library(lubridate)
library(plm)
library(randomForest)

# Import Data
data.full <- read.csv("property.csv")
data.full <- data.full[, c(-1)]

data.full$LandArea <- ifelse(data.full$LandArea == 0, data.full$BuiltUp, data.full$LandArea)
data.full$LandArea <- ifelse(is.na(data.full$LandArea), data.full$BuiltUp, data.full$LandArea)

data.full$no_school_1km <- ifelse(is.na(data.full$no_school_1km), 0, data.full$no_school_1km)
data.full$no_hospital_2km <- ifelse(is.na(data.full$no_hospital_2km), 0, data.full$no_hospital_2km)
data.full$no_train_station_10km <- ifelse(is.na(data.full$no_train_station_10km), 0, data.full$no_train_station_10km)
data.full$no_conv_store_2km <- ifelse(is.na(data.full$no_conv_store_2km), 0, data.full$no_conv_store_2km)
data.full$no_manufacturing_1km <- ifelse(is.na(data.full$no_manufacturing_1km), 0, data.full$no_manufacturing_1km)
data.full$no_police_2km <- ifelse(is.na(data.full$no_police_2km), 0, data.full$no_police_2km)
data.full$no_bus_station_500m <- ifelse(is.na(data.full$no_bus_station_500m), 0, data.full$no_bus_station_500m)
data.full$dist_bus_500m <- ifelse(is.na(data.full$dist_bus_500m), 500, data.full$dist_bus_500m)
data.full$dist_min_police_2km <- ifelse(is.na(data.full$dist_min_police_2km), 2000, data.full$dist_min_police_2km)
data.full$Date <- as.Date(data.full$Date, "%m/%d/%Y")
data.full$Price <- as.numeric(data.full$Price)

str(data.full)

data.full <- data.full %>% dplyr::select(-BuiltUp, -coordinates, -latitude, -longitude, -school.normalised)
data.linear <- na.omit(data.full) 
#data.forest <- data.full
#data.forest[is.na(data.forest)]<-"Null"

### Develop Lasso model
library("glmnet")

x <- model.matrix(Pricepsf ~ .-Address -AddressArea - Sub.Area -Date, data = data.linear)
y <- data.linear$Pricepsf

lasso.property <- glmnet(x, y, alpha = 1)
str(lasso.property)
plot(lasso.property, xvar="lambda", label = TRUE)

# CV for optimal lambda
set.seed(1)
lasso.cv <- cv.glmnet(x, y, alpha=1)
plot(lasso.cv)

# optimal lambda
lasso.lam <- lasso.cv$lambda.min
log(lasso.lam)
points(log(lasso.lam), min(lasso.cv$cvm), cex=3)

lasso.opt <- glmnet(x, y, alpha = 1)
plot(lasso.property, xvar="lambda", label = TRUE)
predict(lasso.property, type="coefficient", s=lasso.lam, exact=TRUE)

# Random Forest
library(randomForest)
 
# tune random forest (mtry) manually
N <- ncol(data.linear) - 9
mse.rfs <- rep(0, N)
for(m in 1:N){
    set.seed(12)
    rf <- randomForest(Pricepsf ~ .-Address -AddressArea - Sub.Area -Date -Price-dis_Klcenter_km - dis_airport_km - no_manufacturing_500, 
                       data = data.linear, mtry=m)
    mse.rfs[m] <- rf$mse[500]
}
plot(1:N, mse.rfs, type="b", xlab="mtry", ylab="OOB Error")


mtry.opt <- which.min(mse.rfs)

# fit a random forest model
set.seed(12)  # RF is random in both the bootstrap part and the predictor selection part
rf <- randomForest(Pricepsf ~ .-Address -AddressArea - Sub.Area -Date - Price - dis_Klcenter_km - dis_airport_km - no_manufacturing_500, 
                   data=data.linear, mtry = mtry.opt) 

# Estimate projected population
historical.population <- read.csv("population.csv")
future.years <- c(2018, 2019, 2020, 2021, 2022)
pp.growth <- c(2.7, 2.7, 2.7, 2.7, 2, 2.7, 2.7, 2.7, 2, 2.7, 2.7, 2.7, 2.7, 2.7, 2.7)
cities <- c("AMPANG JAYA", "BALAKONG", "BATU ARANG", "BERANANG", "BUKIT TINGGI", "GOMBAK", "KAJANG", "KLANG", 
            "PETALING JAYA", "RAWANG", "SELAYANG", "SEMENYIH", "SERENDAH", "SHAH ALAM", "SUBANG JAYA")
future.population <- data.frame(matrix(nrow = length(future.years), ncol = 0))
future.pop.extrapol <- data.frame(matrix(nrow = length(future.years), ncol = 0))
future.pop.bygrowth <- data.frame(matrix(nrow = 0, ncol = length(cities)))
for (x in 1:length(future.years)) {
  if (x == 1) {
    future.pop.bygrowth[x, ] <- historical.population[8, -1] * (1 + pp.growth / 100)  
  } else {
    future.pop.bygrowth[x, ] <- future.pop.bygrowth[x-1, ] * (1 + pp.growth / 100)
  }
}
future.pop.bygrowth <- cbind(future.years, future.pop.bygrowth)
colnames(future.pop.bygrowth) <- c("future.years", cities)
future.pop.bygrowth

future.pop.extrapol <- cbind(future.pop.extrapol, future.years)
for (i in 2:(ncol(historical.population))) {
  pop.mod <- lm(historical.population[[i]] ~ Year, data = historical.population)
  future.pop.extrapol[, cities[i-1]] <- predict(pop.mod, data.frame(Year = future.years))
}
future.pop.extrapol
#future.population <- (future.pop.bygrowth + future.pop.extrapol) / 2
future.population <- future.pop.bygrowth

# Estimate projected CPI
historical.cpi <- read.csv("cpi.csv")
future.cpi <- data.frame(matrix(nrow = length(future.years), ncol = 0))
future.cpi <- cbind(future.years, future.cpi)
for (i in 2:(ncol(historical.cpi))) {
  cpi.mod <- lm(historical.cpi[[i]] ~ Year, data = historical.cpi)
  future.cpi[, cities[i-1]] <- predict(cpi.mod, data.frame(Year = future.years))
}
future.cpi

## Generated predicted property price for 2018
property2018 <- data.linear
property2018$Year <- rep("2018", nrow(property2018))

for (j in 1:nrow(property2018)) {
  property2018[j, "Population"] <- future.population[1, 1+which(cities == property2018[j, "Area"])[1]]
  property2018[j, "CPI"] <- future.cpi[1, 1 + which(cities == property2018[j, "Area"])[1]]
  # Introduce perturbation on proximity of schools
  dist <- property2018[j, "min_dist_school_2km"]
  if (dist > 500) {
    property2018[j, "min_dist_school_2km"] <- ifelse(rbinom(1,1,0.5) == 1, 0.75 * dist, dist)
  }
  dist <- property2018[j, "dist_conv_store_2km"]
  if (dist > 300) {
    property2018[j, "dist_conv_store_2km"] <- ifelse(rbinom(1,1,0.5) == 1, 0.75 * dist, dist)
  }
}
property2018$Pop.Density <- property2018$Population / property2018$CityArea.km2.
property2018$PredictedPrice <- predict(rf, newdata = property2018)
write.csv(property2018, file = "property2018.csv")

# Print predicted property prices for different cities
property.price.2018 <- data.frame(matrix(nrow = length(cities), ncol = 0))
property.price.2018 <- cbind(cities, property.price.2018)
for (k in 1:length(cities)) {
  property.price.2018[k, "avgprice"] <- mean(property2018[property2018$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
property.price.2018

## Generated predicted property price for 2019
property2019 <- property2018
property2019$Year <- rep("2019", nrow(property2019))

for (j in 1:nrow(property2019)) {
  property2019[j, "Population"] <- future.population[2, 1+which(cities == property2019[j, "Area"])[1]]
  property2019[j, "CPI"] <- future.cpi[2, 1 + which(cities == property2019[j, "Area"])[1]]
  # Introduce perturbation on proximity of schools and convenience store
  dist <- property2019[j, "min_dist_school_2km"]
  if (dist > 500) {
    property2019[j, "min_dist_school_2km"] <- ifelse(rbinom(1,1,0.6) == 1, 0.75 * dist, dist)
  }
  dist <- property2019[j, "dist_conv_store_2km"]
  if (dist > 300) {
    property2019[j, "dist_conv_store_2km"] <- ifelse(rbinom(1,1,0.6) == 1, 0.75 * dist, dist)
  }
}
property2019$Pop.Density <- property2019$Population / property2019$CityArea.km2.
property2019$PredictedPrice <- predict(rf, newdata = property2019)
write.csv(property2019, file = "property2019.csv")

# Print predicted property prices for different cities
property.price.2019 <- data.frame(matrix(nrow = length(cities), ncol = 0))
property.price.2019 <- cbind(cities, property.price.2019)
for (k in 1:length(cities)) {
  property.price.2019[k, "avgprice"] <- mean(property2019[property2019$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
property.price.2019

## Generated predicted property price for 2020
property2020 <- property2019
property2020$Year <- rep("2020", nrow(property2020))

for (j in 1:nrow(property2020)) {
  property2020[j, "Population"] <- future.population[3, 1+which(cities == property2020[j, "Area"])[1]]
  property2020[j, "CPI"] <- future.cpi[3, 1 + which(cities == property2020[j, "Area"])[1]]
  # Introduce perturbation on proximity of schools
  dist <- property2020[j, "min_dist_school_2km"]
  if (dist > 500) {
    property2020[j, "min_dist_school_2km"] <- ifelse(rbinom(1,1,0.7) == 1, 0.75 * dist, dist)
  }
  dist <- property2020[j, "dist_conv_store_2km"]
  if (dist > 300) {
    property2020[j, "dist_conv_store_2km"] <- ifelse(rbinom(1,1,0.7) == 1, 0.75 * dist, dist)
  }
}
property2020$Pop.Density <- property2020$Population / property2020$CityArea.km2.
property2020$PredictedPrice <- predict(rf, newdata = property2020)
write.csv(property2020, file = "property2020.csv")

# Print predicted property prices for different cities
property.price.2020 <- data.frame(matrix(nrow = length(cities), ncol = 0))
property.price.2020 <- cbind(cities, property.price.2020)
for (k in 1:length(cities)) {
  property.price.2020[k, "avgprice"] <- mean(property2020[property2020$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
property.price.2020


## Generated predicted property price for 2021
property2021 <- property2020
property2021$Year <- rep("2021", nrow(property2021))

for (j in 1:nrow(property2021)) {
  property2021[j, "Population"] <- future.population[4, 1+which(cities == property2021[j, "Area"])[1]]
  property2021[j, "CPI"] <- future.cpi[4, 1 + which(cities == property2021[j, "Area"])[1]]
  # Introduce perturbation on proximity of schools
  dist <- property2021[j, "min_dist_school_2km"]
  if (dist > 500) {
    property2021[j, "min_dist_school_2km"] <- ifelse(rbinom(1,1,0.8) == 1, 0.75 * dist, dist)
  }
  dist <- property2021[j, "dist_conv_store_2km"]
  if (dist > 300) {
    property2021[j, "dist_conv_store_2km"] <- ifelse(rbinom(1,1,0.8) == 1, 0.75 * dist, dist)
  }
}
property2021$Pop.Density <- property2021$Population / property2021$CityArea.km2.
property2021$PredictedPrice <- predict(rf, newdata = property2021)
write.csv(property2021, file = "property2021.csv")

# Print predicted property prices for different cities
property.price.2021 <- data.frame(matrix(nrow = length(cities), ncol = 0))
property.price.2021 <- cbind(cities, property.price.2021)
for (k in 1:length(cities)) {
  property.price.2021[k, "avgprice"] <- mean(property2021[property2021$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
property.price.2021


## Generated predicted property price for 2021
property2022 <- property2021
property2022$Year <- rep("2022", nrow(property2022))

for (j in 1:nrow(property2022)) {
  property2022[j, "Population"] <- future.population[5, 1+which(cities == property2022[j, "Area"])[1]]
  property2022[j, "CPI"] <- future.cpi[5, 1 + which(cities == property2022[j, "Area"])[1]]
  # Introduce perturbation on proximity of schools
  dist <- property2022[j, "min_dist_school_2km"]
  if (dist > 500) {
    property2022[j, "min_dist_school_2km"] <- ifelse(rbinom(1,1,0.8) == 1, 0.75 * dist, dist)
  }
  dist <- property2022[j, "dist_conv_store_2km"]
  if (dist > 300) {
    property2022[j, "dist_conv_store_2km"] <- ifelse(rbinom(1,1,0.8) == 1, 0.75 * dist, dist)
  }
}
property2022$Pop.Density <- property2022$Population / property2022$CityArea.km2.
property2022$PredictedPrice <- predict(rf, newdata = property2022)
write.csv(property2022, file = "property2022.csv")

# Print predicted property prices for different cities
property.price.2022 <- data.frame(matrix(nrow = length(cities), ncol = 0))
property.price.2022 <- cbind(cities, property.price.2022)
for (k in 1:length(cities)) {
  property.price.2022[k, "avgprice"] <- mean(property2022[property2022$Area == cities[k], ]$PredictedPrice, na.rm=TRUE)
}
property.price.2022


rf
plot(rf)
varImpPlot(rf)
partialPlot(rf, data.linear, x.var= "BuildingType")
partialPlot(rf, data.linear, x.var= "Area")
partialPlot(rf, data.linear, x.var= "Pop.Density")
partialPlot(rf, data.linear, x.var= "min_dist_school_2km")
partialPlot(rf, data.linear, x.var= "LandArea")

png(filename="var importance.png")
varImpPlot(rf)
dev.off()
