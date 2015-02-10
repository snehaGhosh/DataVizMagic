#install.packages("dplyr")

library(lubridate)
library(dplyr)


# read in the comma-delimited data
data <- read.csv(file = "DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

#Format inspection dates as date
data$date <- as.Date(data$INSPECTION.DATE, "%m/%d/%Y")
data$year <- year(data$date)
data$month <- month(data$date)
data$grade_date <- as.Date(data$GRADE.DATE, "%m/%d/%Y")


#Drop incomplete data
data<-data[!(data$year==c(2011, 2010, 2015),]

#Create a dataframe of inspections (each inspection has some lines with and without a grade)
df_inspect = unique(data[,c("CAMIS","date", "month", "year", "GRADE", "grade_date")])
#Keep only inspections with letter grades
df_inspect = subset(df_inspect, GRADE %in% c("A","B","C"))

#Collapse counts by year/month
df_inspect$A_grade[df_inspect$GRADE == "A"] <- 1
df_inspect$B_grade[df_inspect$GRADE == "B"] <- 1
df_inspect$C_grade[df_inspect$GRADE == "C"] <- 1

monthyear<-group_by(df_inspect, year, month)
counts_by_month <- summarise(monthyear,
  countA = sum(A_grade, na.rm=TRUE),
  countB = sum(B_grade, na.rm=TRUE),
  countC = sum(C_grade, na.rm=TRUE)
)

#Calculate totals and percentages
mutate(counts_by_month,
	total_count = countA + countB + countC,
	perc_A = countA/total_count,
	perc_B = countB/total_count,
	perc_C = countC/total_count)	

