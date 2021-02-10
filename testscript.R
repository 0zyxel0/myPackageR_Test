library(myRFMcalculator)
library(data.table)
library(lubridate)
library(ggplot2)
library(Hmisc)



# Read in the data transactions.csv. ####
transactions <- fread("data/transactions.csv")

#Check the data format
str(transactions)

#date in POSIXct format. ####
transactions[, TransDate:=dmy(TransDate, tz = "UTC")]


#Check the data format
str(transactions)


temp <- myRFMcalculator::calculate_RFM(transactions,60,20,20)
table(temp$group)

