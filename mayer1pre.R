#mayer table 2
library(stargazer)

#percents across just D
anes_prevoter <- subset(anes_2008, anes_2008$prevoter==TRUE)

dummiespre <- dummy(anes_prevoter$subbucketsfac, sep="", fun= as.numeric, drop=FALSE)

weightspre <- anes_prevoter$V080102

countspre <- colSums(dummiespre)

dumweightpre <- sapply(1:ncol(dummiespre),function(x) dummiespre[,x] * weightspre )
sumspre <- colSums(dumweightpre)
totalpre <- sum(sumspre)

range <- c("-100:-91", "-90:-81", "-80:-71", " -70:-61", "-60:-51", "-50:-41", "-40:-3","-30:-21", "-20:-16", "15:-11", "-10:-6", "-5:-1", "0", "1:5",  "6:10", "11:15" ,"16:20", "21:30", "31:40", "41:50",  "51:60", "61:70", "71:80","81:90", "91:100", "NA")

wpctpre <- (sumspre/totalpre)*100
percentpre <- round(wpctpre, digits=2)
names(percent) <- range

mayer1pre <- cbind(range, countspre, percentpre)

stargazer(mayer1pre, rownames=FALSE)


