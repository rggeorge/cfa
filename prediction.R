library(caret)

dat <- read.csv("Violations-2012.csv")
dat$date <- ymd_hms(dat$violation_date)

pred <- function(viol_type){
  
  dates <- dat$date[dat$violation_type=="Refuse Accumulation"]
  
  
}