#percent change in vote choice, pre-post election
library(car)
library(dplyr)
#of postvoters, how many changed party?

#pre votechoice is V083169a  1. Barack Obama 2. John McCain
#postvote choice is V085044a 1. Barack Obama 2. John McCain


summary(anes_2008$V083169a)  
anes_2008$votepre <- recode(anes_2008$V083169a, " '1. Barack Obama'=1 ; '2. John McCain'=0; '7. Other {SPECIFY}'=3; else=NA ", as.factor.result=FALSE)
str(anes_2008$votepre)
summary(anes_2008$V085044a)
anes_2008$votepost <- recode(anes_2008$V085044a, " '1. Barack Obama'=1 ; '3. John McCain'=0; '7. Other {SPECIFY}'=3; else=NA ", as.factor.result=FALSE)
str(anes_2008$votepost)

anes_2008$voteswitch <- ifelse(anes_2008$votepre==anes_2008$votepost, 0, 1)
str(anes_2008$voteswitch)
summary(anes_2008$voteswitch)

switchers <- subset(anes_2008, voteswitch==1, select = c(votepre, votepost, V083037b, V083037a, V080102, subbucketsfac, cand_therm_sub))
stargazer(switchers)

switchDs <- count(switchers$V083037a)
switchDs
switchRs <- count (switchers$V083037b)
switchRs
switchsub <- count(switchers$cand_therm_sub)
switchsub

