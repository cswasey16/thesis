library(foreign)
anes_2008 <- read.dta("anes_timeseries_2008_stata12.dta")
View(anes_2008)
str(anes_2008)
#Variables of interest
library(DescTools)
library(plyr)
library(weights)
library(dplyr)
library(dummies)
library(car)

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


anes_2008$subbuckets <- anes_2008$cand_therm_sub

anes_2008$subbucketsfac <- recode(anes_2008$subbuckets, "-100:-91='A' ; -90:-81='B' ; 
                                  -80:-71='C'; -70:-61='D' ; -60:-51='E' ; -50:-41='F'; -40:-31='G' ;
                                  -30:-21='H'; -20:-16='I' ; -15:-11='J'; -10:-6='K'; -5:-1='L' ;
                                  0='M' ; 1:5='N'; 6:10='O'; 11:15='P'; 16:20='Q'; 21:30='R' ;
                                  31:40='S'; 41:50='T'; 51:60='U'; 61:70='V'; 71:80='W';
                                  81:90='X'; 91:100='Y' ; else='Z'", as.factor.result = TRUE)

str(anes_2008$subbucketsfac)

#vars to keep
#weight D therm R therm diff subbucketsfac votepre votepost partyID ideo

# 



names <- paste("subbucketsfac", letters, sep="")
# names2 <- paste("therm", letters, sep="")
# 
attach(mayerbucketsdummy)
list <- c(paste("subbucketsfac", letters, sep=""))


numeric <- function(x){
x <- as.numeric(x)
}

numeric(list[1])
        
weight <- function(x){
  print(x)
  mayerbucketsdummy$x <- mayerbucketsdummy$x*mayerbucketsdummy$V080102
  print(x)
}

weight(list[1])

for(i in list){
  print(i)
  print("beep")
  weight(i)
}



# #for(i in 1:4){
#   print(list[i])
# # }
# paste("dtherm", A, sep="")
# lapply(list, )
# $mayerbucketsdummy$dtherm (i) <- mayerbucketsdummy$subbucketsfac (i) * mayerbucketsdummy$V080102

 
mayerbucketsdummy$dthermA <- mayerbucketsdummy$subbucketsfacA*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermA) <- "-100 to -91"
mayerbucketsdummy$dthermB <- mayerbucketsdummy$subbucketsfacB*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermB) <- "-90 to -81"
mayerbucketsdummy$dthermC <- mayerbucketsdummy$subbucketsfacC*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermC) <- "-80 to -71"
mayerbucketsdummy$dthermD <- mayerbucketsdummy$subbucketsfacD*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermD) <- "-70 to -61"
mayerbucketsdummy$dthermE <- mayerbucketsdummy$subbucketsfacE*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermE) <- "-60 to -51"
mayerbucketsdummy$dthermF <- mayerbucketsdummy$subbucketsfacF*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermF) <- "-50 to -41"
mayerbucketsdummy$dthermG <- mayerbucketsdummy$subbucketsfacG*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermG) <- "-40 to -31"
mayerbucketsdummy$dthermH <- mayerbucketsdummy$subbucketsfacH*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermH) <- "-30 to -21"
mayerbucketsdummy$dthermI <- mayerbucketsdummy$subbucketsfacI*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermI) <- "-20 to -16"
mayerbucketsdummy$dthermJ <- mayerbucketsdummy$subbucketsfacJ*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermJ) <- "-15 to -11"
mayerbucketsdummy$dthermK <- mayerbucketsdummy$subbucketsfacK*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermK) <- "-10 to -6"
mayerbucketsdummy$dthermL <- mayerbucketsdummy$subbucketsfacL*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermL) <- "-5 to -1"
mayerbucketsdummy$dthermM <- mayerbucketsdummy$subbucketsfacM*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermM) <- "0"
mayerbucketsdummy$dthermN <- mayerbucketsdummy$subbucketsfacN*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermN) <- "1 to 5"
mayerbucketsdummy$dthermO <- mayerbucketsdummy$subbucketsfacO*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermO) <- "6 to 10"
mayerbucketsdummy$dthermP <- mayerbucketsdummy$subbucketsfacP*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermP) <- "11 to 15"
mayerbucketsdummy$dthermQ <- mayerbucketsdummy$subbucketsfacQ*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermQ) <- "16 to 20"
mayerbucketsdummy$dthermR <- mayerbucketsdummy$subbucketsfacR*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermR) <- "21 to 30"
mayerbucketsdummy$dthermR <- mayerbucketsdummy$subbucketsfacS*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermS) <- "31 to 40"
mayerbucketsdummy$dthermT <- mayerbucketsdummy$subbucketsfacT*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermT) <- "41 to 50"
mayerbucketsdummy$dthermU <- mayerbucketsdummy$subbucketsfacU*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermU) <- "51 to 60"
mayerbucketsdummy$dthermV <- mayerbucketsdummy$subbucketsfacV*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermV) <- "61 to 70"
mayerbucketsdummy$dthermW <- mayerbucketsdummy$subbucketsfacW*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermW) <- "71 to 80"
mayerbucketsdummy$dthermX <- mayerbucketsdummy$subbucketsfacX*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermX) <- "81 to 90"



mayerbucketsdummy$dthermY <- mayerbucketsdummy$subbucketsfacY*mayerbucketsdummy$V080102
names(mayerbucketsdummy$dthermY) <- "91 to 100"


for(i in 1:26){
  print(sum(mayerbucketsdummy$dtherms[i]))
}

for(i in 1:26){
  counts[i] <- sum(mayerbucketsdummy$dtherms[i])
  
}
A <- sum(mayerbucketsdummy$dthermA)
B <- sum(mayerbucketsdummy$dthermB)




sum(dthermA)



# 
# 
# 
# 
# summary(mayerbucketsdummy$dthermA)


#construct divided Mayer metric
#anes_2008$mayerbuckets <- NA

#anes_2008$mayer15 <- ifelse(anes_2008$cand_therm_sub < 16 & anes_2008$cand_therm_sub > -16, TRUE, FALSE)
#mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)

#print(wpct(anes_2008$mayerbuckets, weight=anes_2008$V080102a) *100)


# mayer15 <- NA
# mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)
