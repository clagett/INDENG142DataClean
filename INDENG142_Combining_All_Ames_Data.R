# Turning quarterly data about Ames, Iowa into to monthly values. 
# Monthly values are simply quarterly values repated across each contained month.
# Data source: https://fred.stlouisfed.org/tags/series?t=ames%3Bia%3Bquarterly&ob=f&od=asc


# Reading in files
file1 <- read.csv("Ames Quarterly Total Wages.csv")
file2 <- read.csv("All-Transactions House Price Index for Ames.csv")
file3 <- read.csv("Ames Quarterly Weekly Wages for Employees in Private Establishments.csv")
file4 <- read.csv("Ames Quarterly Weekly Wages for Employees in Total Covered Establishments.csv")
file5 <- read.csv("Average Weekly Wages for Employees in Federal Government Establishments in Ames.csv")

# Function repeats each value in each row 3 times (Quarterly -> Monthly)
# It is noted that for variables like "Total Wages" for each quarter, it would be more correct to divide the date
# however, doing so would have no different of an impact on our model, as numerical ratios remain the same.
convert <- function(x){
     newdata <- c()
     for(i in 1:nrow(x)){
          newdata <- append(newdata, rep(x[i,2], 3))
     }
     newdata
}

# Removing the final month of data, as the rest of our data we are using only spans until August.
Ames_Total_Wages <- convert(file1)[-57]
All_Transactions_House_Price_Index <- convert(file2)[-57]
Ames_Weekly_Wages_Private_Establishments <- convert(file3)[-57]
Ames_Weekly_Wages_Total_Covered_Establishments <- convert(file4)[-57]
Ames_Weekly_Wages_Federal_Government_Establishments <- convert(file5)[-57]

# Copying the vector containing the months we are using from another dataset
dates <- read.csv("Unemployment.csv")[1]

# Combined all new monthly data into one data frame with a row to label months (and for using to merge with other data)
table <- cbind(dates, 
               Ames_Total_Wages, 
               All_Transactions_House_Price_Index, 
               Ames_Weekly_Wages_Private_Establishments, 
               Ames_Weekly_Wages_Total_Covered_Establishments, 
               Ames_Weekly_Wages_Federal_Government_Establishments)
View(table)


write.csv(table, file="ConvertedQuarterData.csv")


# Combining monthly measured values from separate files into one table. The files must be in their own distinct working directory.
files <- list.files()

monthly_data <- dates
for(i in 1:length(files)){
     data <- read.csv(files[i])[2]
     monthly_data <- cbind(monthly_data, data)
}
View(monthly_data)


# Combining columns from converted quarterly data with those originally measured in monthly, for a table with complete variables.
full_data <- cbind(table, monthly_data[-1])
View(full_data)
