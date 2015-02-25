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

###############################
######Time series of grades
################################
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
graph_a<-ggplot() + 
  geom_line(data = d1, aes(x = date, y = total_count)) +
  xlab('Date') +
  ylab('Total number of inspections') + 
  labs(title="Total number of inspections by month")


#Plot share of inspections by grade
dd_sub = counts_by_month[,c("date", "perc_A", "perc_B", "perc_C")]
graph_b<-ggplot(data = counts_by_month, aes(x = date)) +
  geom_line(aes(y = perc_A, colour = "A grade")) +
  geom_line(aes(y = perc_B, colour = "B grade")) +
  geom_line(aes(y = perc_C, colour = "C grade")) +
  scale_colour_manual("", 
                      breaks = c("A grade", "B grade", "C grade"),
                      values = c("red", "green", "blue")) +
  xlab(" ") +
  scale_y_continuous("Share of inspections", limits = c(0,1)) + 
  labs(title="Share of inspection results by grade over time")



####################################
#Get number of violations by category
####################################
# read in the classifications file
classifications <- read.csv(file = "Classifications.csv")

data_class <- merge(data, classifications, by="VIOLATION.CODE")
#data_class_selected <-data[,c("VIOLATION.DESCRIPTION", "CLASSIFICATION")]

## set the levels in order we want
data_class <- within(data_class, 
                   CLASSIFICATION <- factor(CLASSIFICATION, 
                                      levels=names(sort(table(CLASSIFICATION), 
                                                        increasing=TRUE))))
## plot
graph_d <-ggplot(data_class,aes(x=CLASSIFICATION))+geom_bar(binwidth=1, fill="blue") + 
  coord_flip() +
  labs(y = "Number of violations") +
  labs(x = "Violation category") +
  ggtitle("Number of violations by category, 2012-2014")  +
  theme(axis.title.x = element_text(size = rel(0.7)), axis.title.x = element_text(size = rel(0.7)))

####################################
#Top violations for c-grade restaurants
####################################
#Keep only the C-violations
data_C<-data[data$GRADE=='C',]

#Keep only the top 10 violations
data_C_collapsed <- data.frame(table(data_C$VIOLATION.DESCRIPTION))
data_C_collapsed <- data_C_collapsed[order(-data_C_collapsed$Freq),]
data_C_collapsed_top10 <- data_C_collapsed[1:10, ]

#Take the first sentence (or everything before first semicolon) from every description
data_C_collapsed_top10$short <- sapply(strsplit(as.character(data_C_collapsed_top10$Var1), "\\."), `[`, 1)
data_C_collapsed_top10$short <- sapply(strsplit(as.character(data_C_collapsed_top10$short), ";"), `[`, 1)

#Put in line breaks at 45th character to display nicely
data_C_collapsed_top10$split <- gsub('(.{1,45})(\\s|$)', '\\1\n', data_C_collapsed_top10$short)

#Plot
graph_e<-ggplot(data_C_collapsed_top10,aes(x=split, y=Freq))+geom_bar(stat="identity", fill="blue") + 
  coord_flip() +
  labs(y = "Number of violations") +
  labs(x = "Violation category") +
  ggtitle("Top violations for C-graded inspections")  +
  theme(axis.title.x = element_text(size = rel(0.7)), axis.title.x = element_text(size = rel(0.7)))



#Print to PDF
pdf(file = "sv_plots.pdf", width = 11, height = 8.5)
print(a)
print(b)
print(graph_d)
print(graph_e)
dev.off()

