library(ggplot2)
library(lubridate)
require(zoo)
dat <- read.csv("Violations-2012.csv")


#visualize 
#barplot(table(dat$violation_category), names.arg=sapply(levels(dat$violation_category), function(x) strtrim(x,13)))
dat$date <- ymd_hms(dat$violation_date)
dat$month <- as.yearmon(dat$date)
#require(zoo)
#dat$month<- as.yearmon(dat$date)
#barplot(table(dat$month), xlab="Month of violation")
#sumdat <- dat[c("month", "violation_category")]

tt <- unlist(tapply(dat$violation_category, dat$month, table))
dates <- factor(unlist(lapply(strsplit(as.character(names(tt)), "\\."), function(x) x[1])))
dts = factor(dates,levels(dates)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
vios <- unlist(lapply(strsplit(as.character(names(tt)), "\\."), function(x) x[2])) 
findat <- data.frame(dts, vios, tt)

ggplot(findat, aes(x = dts, y = tt, colour = vios, group=vios)) + geom_point() + geom_line()

#paragraph on insights



#predictive model

