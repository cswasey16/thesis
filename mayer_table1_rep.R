#replication of Mayer

#table
library(car)
install.packages("plyr")
library(DescTools)
library(plyr)
library(weights)
library(dplyr)

anes_2008$subbuckets <- anes_2008$cand_therm_sub

anes_2008$subbucketsfac <- recode(anes_2008$subbuckets, "-100:-91='A' ; -90:-81='B' ; 
-80:-71='C'; -70:-61='D' ; -60:-51='E' ; -50:-41='F'; -40:-31='G' ;
-30:-21='H'; -20:-16='I' ; -15:-11='J'; -10:-6='K'; -5:-1='L' ;
0='M' ; 1:5='N'; 6:10='O'; 11:15='P'; 16:20='Q'; 21:30='R' ;
31:40='S'; 41:50='T'; 51:60='U'; 61:70='V'; 71:80='W';
81:90='X'; 91:100='Y' ; else='Z'", as.factor.result = TRUE)

str(anes_2008$subbucketsfac)


#count respondents in each bucket
n_sample_buckets <- count(anes_2008$subbucketsfac)
count_buckets <- n_sample_buckets$freq

#weighted percent respondents in each bucket

n_sample_buckets$percent_buckets <- wpct(anes_2008$subbucketsfac, weight = anes_2008$V080102a) * 100


#post vote for prez V085044 1= yes 
anes_2008$postvoter <- NA
anes_2008$postvoter <- ifelse(anes_2008$V085044=="1. Yes, voted for President", TRUE, FALSE)
summary(anes_2008$postvoter)

#pre vote intention V083169
anes_2008$prevoter <- NA
anes_2008$prevoter <- ifelse(anes_2008$V083169=="1. Yes", TRUE, FALSE)
summary(anes_2008$prevoter)

#weighted percent of prevote pop in each bucket (bucket prevote/all prevote)
#total 1983

prevote <- subset(anes_2008, anes_2008$prevoter==TRUE)
str(prevote$subbucketsfac)
prevoters <- count(prevote$subbucketsfac)
prevoters$perc <- wpct(prevote$subbucketsfac, weight=prevote$V080102a)*100




#weighted percent postvote pop in each bucket (bucket postvote/ all postvote)
#total 1593
postvote <- subset(anes_2008, anes_2008$postvoter==TRUE)
str(postvote$subbucketsfac)
postvoters <- count(postvote$subbucketsfac)
postvoters$perc <- wpct(postvote$subbucketsfac, weight=postvote$V080102a)*100


#therm diff, percent of sample, % pre rept voters, % post rept voters


#therm diff, % post voters, POSTELEC % D, % R, % change from PRE, n

