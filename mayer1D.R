#post

#mayer table 2 Democrats
library(stargazer)

#percents across just D
anes_D <- subset(anes_2008, anes_2008$demvote==TRUE)

dummiesD <- dummy(anes_D$subbucketsfac, sep="", fun= as.numeric, drop=FALSE)

weightsD <- anes_D$V080102

countsD <- colSums(dummiesD)

dumweightD <- sapply(1:ncol(dummiesD),function(x) dummiesD[,x] * weightsD )
sumsD <- colSums(dumweightD)
totalD <- sum(sumsD)

range <- c("-100:-91", "-90:-81", "-80:-71", " -70:-61", "-60:-51", "-50:-41", "-40:-3","-30:-21", "-20:-16", "15:-11", "-10:-6", "-5:-1", "0", "1:5",  "6:10", "11:15" ,"16:20", "21:30", "31:40", "41:50",  "51:60", "61:70", "71:80","81:90", "91:100", "NA")

pctvoteD <- (sumsD/sumspost)*100
percentD <- round(pctvoteD, digits=2)
names(percentD) <- range

mayer1D <- cbind(range, countsD, percentD)

stargazer(mayer1D, rownames=FALSE)


