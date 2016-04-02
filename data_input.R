library(foreign)
anes_2008 <- read.dta("anes_timeseries_2008_stata12.dta")
View(anes_2008)
str(anes_2008)
#Variables of interest

library(survey)
#drop unneeded variables
library(weights)
sapply(anes_2008,function(x) sum(is.na(x)))


#V083037a Therm, D prez candidate (Obama)
-6 -8 -9
anes_2008$V083037a [ anes_2008$V083037a < 0] <- NA

#V083037b  Therm, R prez candidaate (McCain)
anes_2008$V083037b [ anes_2008$V083037b < 0] <- NA

#construct Mayer metric

##dplyr (look up)

anes_2008$cand_therm_sub <- anes_2008$V083037b - anes_2008$V083037a  ##R minus D ##

anes_2008$cand_therm_sub [anes_2008$V083037a < 0 | anes_2008$V083037b < 0 ] <- NA
summary(anes_2008$cand_therm_sub)

#construct divided Mayer metric
anes_2008$mayerbuckets <- NA

anes_2008$mayer15 <- ifelse(anes_2008$cand_therm_sub < 16 & anes_2008$cand_therm_sub > -16, TRUE, FALSE)
mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)

print(wpct(anes_2008$mayerbuckets, weight=anes_2008$V080102a) *100)


plot(wpct(anes_2008$mayerbuckets, weight=anes_2008$V080102a), type="h")
weight V080102

mayer15 <- NA
mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)
5