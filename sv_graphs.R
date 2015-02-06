library(lubridate)

# read in the comma-delimited data
data <- read.csv(file = "DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

#Format inspection dates as date
data$date <- as.Date(data$INSPECTION.DATE, "%m/%d/%Y")
data$year <- year(data$date)
data$month <- month(data$date)

#Drop 2011 data (potentially incomplete)
data<-data[!(data$year==2011),]

#Get count of inspections by month
df_countsbymonth <- aggregate(CAMIS ~ year + month, data = data, FUN = length)

