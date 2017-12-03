library(tidyverse)
library(stringr)
library(glmnet)
library(pls)
library(randomForest)
library(caret)
library(leaps)

ames <- read.csv("Ames.csv")
ames$MSSubClass <- as.factor(ames$MSSubClass)

summary(ames$SalePrice)
View(ames)
head(ames)

# This shows the histogram plots of the sales prices
ames %>% ggplot(aes(x = SalePrice)) + geom_histogram(binwidth = 20000)
ames %>% ggplot(aes(x = SalePrice)) + geom_density()
# MAKE IT THE LOG SALES PRICE
ames <- ames %>% mutate(LogSalePrice = log(SalePrice))
ames <- ames %>% select(LogSalePrice, everything())
ames$SalePrice <- NULL

# These show boxplots on how the prices of houses vary based on their overall quality and conditions
ames %>% ggplot(aes(x = as.factor(OverallQual), y = LogSalePrice)) + geom_boxplot()
ames %>% ggplot(aes(x = as.factor(OverallCond), y = LogSalePrice)) + geom_boxplot()

ames %>% ggplot(aes(x = as.factor()))
#########################
# Took Above from class code

# Showing the mean housing prices for ames
colnames(ames)
ames %>% group_by(YrSold)
ames_med <- aggregate(SalePrice~YrSold, data=ames, mean)
ames_mean <- aggregate(SalePrice~YrSold, data=ames, median)

# Now, I want to try to aggregate first by year, then month


# Loading Zillow Data in, will attempt to measure the effect of the drop
zil <- read.csv("City_Zhvi_AllHomes.csv")
z_ames <- zil[647,][184:255]
ames_mean
names(z_ames) <- substring(names(z_ames), 2, 5)

y11 <-sum(z_ames[1:12])/12
y12 <-sum(z_ames[13:24])/12
y13 <-sum(z_ames[25:36])/12
y14 <-sum(z_ames[37:48])/12
y15 <-sum(z_ames[49:60])/12
y16 <-sum(z_ames[61:72])/12

ames_all_yrs <- c(ames_mean$SalePrice, y11, y12, y13, y14, y15, y16)
names(ames_all_yrs) <- c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)

plot(ames_all_yrs, type="l")

# We can see that there was not too much fluxuation in housing prices in the greater picture, compared with that of San Francisco
sf_prices <- zil[706,][124:255]
colSums <- matrix(sf_prices, nrow = 12)
?colSums
sf06 <-sum(sf_prices[1:12]/12)
sf07 <-sum(sf_prices[13:24])/12
sf08 <-sum(sf_prices[25:36])/12
sf09 <-sum(sf_prices[37:48])/12
sf10 <-sum(sf_prices[49:60])/12
sf11 <-sum(sf_prices[61:72])/12
sf12 <-sum(sf_prices[73:84])/12
sf13 <-sum(sf_prices[85:96])/12
sf14 <-sum(sf_prices[97:108])/12
sf15 <-sum(sf_prices[109:120])/12
sf16 <-sum(sf_prices[120:132])/12

sf_all_yrs <- c(sf06, sf07, sf08, sf09, sf10, sf11, sf12, sf13, sf14, sf15, sf16)
names(sf_all_yrs) <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
plot(sf_all_yrs, type="l")

# Not sure why this was necessary because I could have just meausred months, as so:
plot(zil[706,][124:255])

sf_all_yrs
ames_all_yrs

# We can see an obvious drop in the SF data (how to rename the X ticks)
# Now I will normalize the data so I can compare the two
plot(scale(sf_all_yrs), type="l")
lines(scale(ames_all_yrs))

# The mean data, when normalized in both cities, follow the same pattern of deviating from the mean.

rel_years <- zil[124:255]
head(rel_years)

# I want to write a function that sums every 12 consecutive columns for each row, and returns the resulting 11 column dataset

# Now, I am going to measure the magnetude of the price drops in each of the cities,
# First, I created a function that measures the magnetude of the drop by percent the change
# in the highest point in the first 3 measured years against the lowest point measured.
drop_percent <- function(x){
     return((max(x[1:36])
             -min(x))
            /max(x[1:36]))
}

yrs_test <- rel_years[1:5,]
yrs_test

# I then applied the function to all of the rows in the dataset.
drop_vec <- apply(rel_years, 1, drop_percent)

# I take this measurement for the magnetude in price drop, and add it to the original table with the cities
city_pd <- cbind(zil[1:6], drop_vec)
head(city_pd)

# Lets do a little EDA on which cities experienced the largest drops
max(city_pd$drop_vec, na.rm=TRUE)

city_pd <- arrange(city_pd, by=drop_vec)
biggest_drops <- arrange(city_pd, by=desc(drop_vec))[1:10,]
biggest_drops

# It looks like both Florida and California were hit. 
# Going to try to use one of the geographically descriptive columns to make a heatmap of drops with Google Maps

# Loading in a dataset that contains the coordinates for the cities that I want to plot
# I will match each city's coordinates and put them in a table so that I can map each point
# http://geolite.maxmind.com/download/geoip/database/GeoLiteCity_CSV/GeoLiteCity-latest.zip
coords <- read.csv("GeoLiteCity-Location.csv", skip=1)
head(coords)
head(city_pd)

#This dataset has 890,521 different observations. That is a lot of cities. Let's reduce it to the US.
us <- subset(coords, country == "US")

# Next, I will add another two columns to my city_pd dataset, giving the coordinates of the city.
# renaming the city column for lookup
colnames(us)[4] <- "Metro"
lat <- us[c(4, 6)]
lon <- us[c(4, 7)]

base1 <- (merge(lat, us, by = 'Metro'))
head(base1)

city_pd$lon <- with(lon, longitude[match(city_pd$Metro,Metro)])
city_pd$lat <- with(lat, latitude[match(city_pd$Metro, Metro)])
city_pd

# Full records
head(city_pd)
full <- !is.na(city_pd$lon | city_pd$lat)
count(full)

# There are a lot of full records, so we can use this to plot with the other rate
mappoints <- city_pd[full,][7:9]
mappoints2 <- mappoints[!is.na(mappoints$drop_vec),]
x <- mappoints$drop_vec2 / max(mappoints$drop_vec)
x <- x[!is.na(x)]

# This plot shows the severity of where the housing crises hit.
cr <- colorRamp(c("blue", "red"))
map <- qmap("Kansas", zoom=4, maptype="hybrid")
map + geom_point(data = mappoints2, aes(x = lon, y = lat),
                 color = rgb(cr(x), max=255), size = 6, alpha=0.4)


# I can later create a map with more data points from just this coords data  from usand a larger Zillow city dataset

ts <- zil[c(4,7:264)]
latlon <- us[c(4,6,7)]

newtab <-(merge(latlon, ts, by = 'Metro'))

