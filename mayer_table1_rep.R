#replication of Mayer

#table
library(car)
install.packages("plyr")
library(DescTools)
library(plyr)
library(weights)
library(dplyr)
library(dummies)

anes_2008$dummy <- dummy.data.frame(anes_2008$subbucketsfac)

anes_2008$demvote <- ifelse(anes_2008$V085044a=="1. Barack Obama", TRUE, FALSE)
anes_2008$repvote <- ifelse(anes_2008$V085044a=="3. John McCain", TRUE, FALSE)

summary(anes_2008$demvote)

#count respondents in each bucket
n_sample_buckets <- count(anes_2008$subbucketsfac)
count_buckets <- n_sample_buckets$freq

#weighted percent respondents in each bucket

n_sample_buckets$percent_buckets <- wpct(anes_2008$subbucketsfac, weight = anes_2008$V080102a) * 100


#post vote for prez V085044 1= yes 
anes_2008$postvoter <- NA
anes_2008$postvoter <- ifelse(anes_2008$V085044=="1. Yes, voted for President", TRUE, FALSE)
summary(anes_2008$postvoter)

postvote$demvote <- ifelse(postvote$V085044a=="1. Barack Obama", TRUE, FALSE)
postvote$repvote <- ifelse(postvote$V085044a=="3. John McCain", TRUE, FALSE)


#pre vote intention V083169
anes_2008$prevoter <- NA
anes_2008$prevoter <- ifelse(anes_2008$V083169=="1. Yes", TRUE, FALSE)
summary(anes_2008$prevoter)

#weighted percent of prevote pop in each bucket (bucket prevote/all prevote)
#total 1983

prevote <- subset(anes_2008, anes_2008$prevoter==TRUE)
str(prevote$subbucketsfac)
prevoters <- count(prevote$subbucketsfac)
prevoters$perc <- wpct(prevote$subbucketsfac, weight=prevote$V080102)*100



#weighted percent postvote pop in each bucket (bucket postvote/ all postvote)
#total 1593
postvote <- subset(anes_2008, anes_2008$postvoter==TRUE)
str(postvote$subbucketsfac)
postvoters <- count(postvote$subbucketsfac)
postvoters$perc <- wpct(postvote$subbucketsfac, weight=postvote$V080102)*100


#therm diff, percent of sample, % pre rept voters, % post rept voters
 #combination of postvoters, prevoters, and n_sample_buckets

#therm diff, % post voters, POSTELEC % D, % R, % change from PRE, n
post_percents <- count(postvote$subbucketsfac)
post_percents$perc <- wpct(postvote$subbucketsfac, weight=postvote$V080102a)*100

#anes_2008$demsubbucketsfac <- as.factor(ifelse(anes_2008$demvote==TRUE, anes_2008$subbucketsfac, NA))
## ugh ## anes_2008$demsubbucketsfac <- as.factor( anes_2008$dems <- ifelse(anes_2008$demvote==1, anes_2008$subbucketsfac, NA) labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"))
#summary(anes_2008$demsubbucketsfac)

postvote$dembucketsfac <- postvote$subbucketsfac
postvote$dembucketsfac [postvote$demvote==FALSE] <- "Z"
summary(postvote$dembucketsfac)

postvote$repbucketsfac <- postvote$subbucketsfac
postvote$repbucketsfac [postvote$repvote==FALSE] <- "Z"
summary(postvote$repbucketsfac)




summary(anes_2008$V080102)

#i need % of total bucket population that voted D or R
#so  dembucketsfac/subbucketsfac
#but those are unweighted
#ughhhhh
