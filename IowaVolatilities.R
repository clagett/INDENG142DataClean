library(tidyverse)
library(ggmap)
library(ggplot2)

#-----------------------------------------------------------------------------------

## DATA LOAD & PREP

ames <- read.csv("Ames.csv")
# Created a timeseries for ames, and calculated the volatility
ames_median <- aggregate(SalePrice~MoSold+YrSold, data=ames, median)
ames_vol <- sd(ames_median$SalePrice)/mean(ames_median$SalePrice)

# Created a dataset with just longitudes and latitudes of cities in Iowa
IA <- read.csv("GeoLiteCity-Location.csv", skip=1) %>% 
     subset(country == "US") %>% 
     subset(region == "IA")
colnames(IA)[4] <- "Metro"

# Loading time series data for median house prices in cities from Zillow, subsetting Iowa 2006-2010
zil <- (read.csv("City_Zhvi_AllHomes.csv") %>% 
             subset(State == "IA"))[c(4, 124:178)]

#------------------------------------------------------------------------------------------------

## PLOTTING THE VOLATILITIES OF CITIES USING zil and IACoords datasets


# Calculating the Volatility of housing prices of each city in Iowa from the Zillow set
vola <- function(x){
     sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)
}
vol <- (zil[-1] %>% apply(1, sd_na))
zil_cit <- zil[!is.na(vol),][1]
volatilities <- cbind(zil_cit, vol[!is.na(vol)])
colnames(volatilities) <- c("Metros", "Vol")
volatilities

# I want to get rid of duplicate cities, so I average the volatility in each entry for city. This is probably bad. 
cities <- aggregate(volatilities$Vol, by=list(Metros=volatilities$Metro), FUN=mean)
cities


# Now, I create a table from the coordinates table with all Iowa cities and their coordinates
lat <- aggregate(IA$latitude, by=list(Metro=IA$Metro), FUN=mean)
long <- aggregate(IA$longitude, by=list(Metro=IA$Metro), FUN=mean)
IACoords <- cbind(lat, long[2])
IACoords

complete <- merge(cities, IACoords, by.x="Metros", by.y="Metro")[-1]
colnames(complete) <- c("Volatility", "Latitude", "Longitude")


# Plotting volatilities in each city location with ggmap
cr <- colorRamp(c("blue", "red"))
map <- qmap(location="Iowa", zoom=4, maptype="hybrid")
map + geom_point(data = complete, 
                 aes(x = lat, y = lon),
                 color = rgb(cr(x), max=255), 
                 size = 6, 
                 alpha=0.4)

