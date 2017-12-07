# Turning quarterly data to monthly

file1 <- read.csv("Ames Quarterly Total Wages.csv")
file2 <- read.csv("All-Transactions House Price Index for Ames.csv")
file3 <- read.csv("Ames Quarterly Weekly Wages for Employees in Private Establishments.csv")
file4 <- read.csv("Ames Quarterly Weekly Wages for Employees in Total Covered Establishments.csv")
file5 <- read.csv("Average Weekly Wages for Employees in Federal Government Establishments in Ames.csv")


convert <- function(x){
     newdata <- c()
     for(i in 1:nrow(x)){
          newdata <- append(newdata, rep(x[i,], 3))
     }
     newdata
}

one <- convert(file1)[-57]
two <- convert(file2)[-57]
three <- convert(file3)[-57]
four <- convert(file4)[-57]
five <- convert(file5)[-57]

dates <- read.csv("Unemployment.csv")[1]

table <- cbind(dates, one, two, three, four, five)
View(table)
