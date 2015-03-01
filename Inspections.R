library(ggplot2)
library(sqldf)
library("lubridate")

#Set workging directory

setwd("/Users/kresnadi/Documents/Northwestern/predict 490/GroupProject")

chi_inspections <- read.csv(file = "CHI_Food_Inspections.csv")
attach(chi_inspections)
unique(chi_inspections$Facility.Type)

chi_inspections.tmp <- subset(chi_inspections, subset = (Facility.Type == "Restaurant"))
ggplot.object <- ggplot(data=chi_inspections.tmp) + aes(x=Results, fill = Results) + geom_bar()   + labs(y = "Number of Inspections") + ggtitle("Restaurant Inspection Results")
print(ggplot.object)

pdf(file = "Restaurant_Inspection_Results.pdf", width = 11, height = 8.5)
print(ggplot.object)
dev.off()

#NYC dataset

nyc_inspections <- read.csv(file = "NYC_Food_Inspections.csv")

colnames(nyc_inspections) <- gsub("\\.","_",colnames(nyc_inspections))
nyc_inspections$date <- as.Date(nyc_inspections$INSPECTION_DATE,"%m/%d/%Y")
nyc_inspections$yr <- year(nyc_inspections$date)
nyc_inspections$mo <- month(nyc_inspections$date)
nyc_inspections$yrmo <- paste(nyc_inspections$yr,nyc_inspections$mo, sep = "-")

head(nyc_inspections)
attach(nyc_inspections)

# aggregate by cuisine
aggdata <-aggregate(nyc_inspections, by=list(CUISINE_DESCRIPTION,CRITICAL_FLAG), 
                    FUN=length)
aggdataTotCuisine <-aggregate(nyc_inspections, by=list(CUISINE_DESCRIPTION), 
                    FUN=length)
aggdatacombine <- merge(aggdata[,1:3],aggdataTotCuisine[,1:2], by="Group.1")
aggdatacombine$pct <-aggdatacombine$CAMIS.x / aggdatacombine$CAMIS.y
attach(aggdatacombine)
aggdatacombine <- aggdatacombine[order(Group.1,Group.2),]

# Average critical violations by year
aggbyyrbiz <- sqldf("select yr as year, CAMIS, count(*) as mycount from nyc_inspections where yrmo <> '1900-1' and CRITICAL_FLAG = 'Critical' and 
                      yr in (2010,2011,2012,2013,2014)  group by year , CAMIS")
aggbyyrmean <-sqldf("select year, avg(mycount) as avgcount from aggbyyrbiz group by year")
ggplot(aggbyyrmean, aes(x=year, y=avgcount)) + geom_line() + labs(y="Avg. Critical Violations")

# Average critical violations by date
aggbyyrbiz <- sqldf("select date, CAMIS, count(*) as mycount from nyc_inspections where yrmo <> '1900-1' and CRITICAL_FLAG = 'Critical' and 
                    yr in (2010,2011,2012,2013,2014)  group by date , CAMIS")
aggbyyrmean <-sqldf("select date, avg(mycount) as avgcount from aggbyyrbiz group by date")
ggplot(aggbyyrmean, aes(x=date, y=avgcount)) + geom_line() + labs(y="Avg. Critical Violations")


# Breakdown letter grade by borough
aggltrbyborough <- sqldf("select boro, grade, count(*) as count from nyc_inspections where grade in ('A','B','C') and boro <> 'Missing' and 
                      yr in (2010,2011,2012,2013,2014) group by boro, grade")
aggbyborough <- sqldf("select boro, sum(count) as totcount from aggltrbyborough group by boro")
ltrbyborough <- merge(aggltrbyborough, aggbyborough, by="BORO")
ltrbyborough$pct <- ltrbyborough$count / ltrbyborough$totcount
ggplot(ltrbyborough, aes(x=BORO, y=pct, fill=GRADE)) + geom_bar(stat="identity") + coord_flip() + labs(x="Borough") + labs(y="Letter Grade Percentages")

# Breakdown letter grade by cuisine
aggltrbycuisine <- sqldf("select cuisine_description, grade, count(*) as count from nyc_inspections where grade in ('A','B','C') and boro <> 'Missing' and 
                      yr in (2010,2011,2012,2013,2014) group by cuisine_description, grade")
aggbycuisine <- sqldf("select cuisine_description, sum(count) as totcount from aggltrbycuisine group by cuisine_description")
ltrbycuisine <- merge(aggltrbycuisine, aggbycuisine, by="CUISINE_DESCRIPTION")
ltrbycuisine$pct <- ltrbycuisine$count / ltrbycuisine$totcount  
ggplot(ltrbycuisine, aes(x=CUISINE_DESCRIPTION, y=pct, fill=GRADE)) + geom_bar(stat="identity") + coord_flip() + labs(x="Cuisine") + labs(y="Letter Grade Percentages")

#top 10 grade A
ltrbycuisineA <- sqldf("select * from ltrbycuisine where GRADE = 'A' and count > 50 and CUISINE_DESCRIPTION not in ('Not Listed/Not Applicable', 'Other') order by pct desc")
ltrbycuisineA <- ltrbycuisineA[1:10,]
ltrbycusisineTop10 <- sqldf("select a.*, b.pct as maxpct from ltrbycuisine a inner join ltrbycuisineA b  on a.CUISINE_DESCRIPTION = b.CUISINE_DESCRIPTION")
ggplot(ltrbycusisineTop10, aes(x=reorder(CUISINE_DESCRIPTION, maxpct), y=pct, fill=GRADE)) + geom_bar(stat="identity") + coord_flip() + labs(x="Cuisine") + labs(y="Letter Grade Percentages")

ltrbycuisineC <- sqldf("select * from ltrbycuisine where GRADE = 'C' and count > 50 and CUISINE_DESCRIPTION <> 'Not Listed/Not Applicable' order by pct desc")
ltrbycuisineC <- ltrbycuisineC[1:10,]
ltrbycusisineBottom10 <- sqldf("select a.*, b.pct as maxpct from ltrbycuisine a inner join ltrbycuisineC b  on a.CUISINE_DESCRIPTION = b.CUISINE_DESCRIPTION")
ggplot(ltrbycusisineBottom10, aes(x=reorder(CUISINE_DESCRIPTION, maxpct), y=pct, fill=GRADE)) + geom_bar(stat="identity") + coord_flip() + labs(x="Cuisine") + labs(y="Letter Grade Percentages")


myplot <- ggplot(aggdatacombine, aes(x=Group.1, y=pct, fill=Group.2)) + geom_bar(stat="identity") + coord_flip() + labs(x="Cuisine") + labs(y="Critical Flag Percentages")
myplot1 <- ggplot(aggbyyrmean, aes(x=year, y=avgcount)) + geom_line() + labs(y="Avg. Critical Violations")

# print to pdf file for use in many applications
pdf(file = "NYCViolations.pdf", width = 8.5, height = 11)
print(myplot)
print(myplot1)
dev.off()



