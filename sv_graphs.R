#install.packages("dplyr")

library(lubridate)
library(dplyr)

######Data processing

# read in the comma-delimited data
data <- read.csv(file = "DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

#Format inspection dates as date
data$date <- as.Date(data$INSPECTION.DATE, "%m/%d/%Y")
data$year <- year(data$date)
data$month <- month(data$date)
data$grade_date <- as.Date(data$GRADE.DATE, "%m/%d/%Y")


#Drop incomplete data
data<-data[!(data$year==2010) & !(data$year==2011) & !(data$year==2015),]

#Create a dataframe of inspections (each inspection has some lines with and without a grade)
df_inspect = unique(data[,c("CAMIS","date", "month", "year", "GRADE", "grade_date")])
#Keep only inspections with letter grades
df_inspect = subset(df_inspect, GRADE %in% c("A","B","C"))

df_inspect$A_grade[df_inspect$GRADE == "A"] <- 1
df_inspect$B_grade[df_inspect$GRADE == "B"] <- 1
df_inspect$C_grade[df_inspect$GRADE == "C"] <- 1


######Time series of grades
#Collapse counts by year/month

monthyear<-group_by(df_inspect, year, month)
counts_by_month <- summarise(monthyear,
  countA = sum(A_grade, na.rm=TRUE),
  countB = sum(B_grade, na.rm=TRUE),
  countC = sum(C_grade, na.rm=TRUE)
)

#Calculate totals and percentages
counts_by_month<- mutate(counts_by_month,
	total_count = countA + countB + countC,
	perc_A = countA/total_count,
	perc_B = countB/total_count,
	perc_C = countC/total_count)	

#Re-build a date variable
counts_by_month$date <- ISOdate(counts_by_month$year, counts_by_month$month, 1)

#Make it a time series
totals_ts <- ts(counts_by_month$total_count,
  frequency=12,
  start=c(year=2012, month=1))

#To plot the time series
d1 <- counts_by_month[, c("date", "total_count")]
d2 <- counts_by_month[, c("date", "perc_A")]
d3 <- counts_by_month[, c("date", "perc_B")]
d4 <- counts_by_month[, c("date", "perc_C")]


#Plot the total number of inspections
a<-ggplot() + 
  geom_line(data = d1, aes(x = date, y = total_count)) +
  xlab('Date') +
  ylab('Total number of inspections') + 
  labs(title="Total number of inspections by month")


#Plot share of inspections by grade
dd_sub = counts_by_month[,c("date", "perc_A", "perc_B", "perc_C")]
b<-ggplot(data = counts_by_month, aes(x = date)) +
  geom_line(aes(y = perc_A, colour = "A grade")) +
  geom_line(aes(y = perc_B, colour = "B grade")) +
  geom_line(aes(y = perc_C, colour = "C grade")) +
  scale_colour_manual("", 
                      breaks = c("A grade", "B grade", "C grade"),
                      values = c("red", "green", "blue")) +
  xlab(" ") +
  scale_y_continuous("Share of inspections", limits = c(0,1)) + 
  labs(title="Share of inspection results by grade over time")


#Print to PDF
pdf(file = "sv_plots.pdf", width = 11, height = 8.5)
print(a)
print(b)
dev.off()

