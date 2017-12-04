## It is my assumption that there will be a lot of error in predicting housing prices on the Ames set, because
## The mean housing sale price is NOT completely representative of the ACTUAL mean sale price due to low size
# Here, we will plot the median house sale prices

ames <- read.csv("Ames.csv")
# Created a timeseries for ames, and calculated the volatility
ames_median <- aggregate(SalePrice~MoSold+YrSold, data=ames, median)

# Loading the median home records from differenet neighborhoods in Ames from Zillow
zil2 <- (read.csv("City_Zhvi_AllHomes.csv"))[c(4, 124:178)]
zil_com <- zil2[complete.cases(zil2),]
zil_ames <- zil_com[zil_com$Metro == "Ames",]

# Just created a single table with both Ames dataset data and Zillow Ames data
# Probably not needed, just like working with one set though
# Named each with numbers, might want to rename with each neighborhood
full <- cbind(ames_median, t(zil_ames[-1]))[-c(1,2)]
colnames(full) <- c("OriginalAmes", "One", "Two", "Three", "Four", "Five", "Six", "Seven")

# Plotting the timseries data from the Ames dataset with other counties from Ames
plot(ames_median$SalePrice, type = "l", xlab = "Month", ylab = "Price", ylim = c(10000, 300000), col = "red")
lines(full$One)
lines(full$Two)
lines(full$Three)
lines(full$Four)
lines(full$Five)
lines(full$Six)
lines(full$Seven)

# We can see that there are some irrational differences between the median sale prices of each month in our data
abs(ames_median$SalePrice[10]-ames_median$SalePrice[11])/ames_median$SalePrice[10]
# That is a 46.7% price spike in one month. 

# Just to see if taking the mean of the home values is any better, I plotted mean
ames_mean <- aggregate(SalePrice~MoSold+YrSold, data=ames, mean)
lines(ames_mean$SalePrice, col = "blue")
# This looks just as bad, and even higher price values. 
# More than likely, our analysis would better predict the median prices in the Ames set
# because the price fluxations in the Ames set are due to small sample size
# and the differences in the microeconomic variables