#post

#mayer table 2
library(stargazer)


anes_postvoter <- subset(anes_2008, anes_2008$postvoter==TRUE)

dummiespost <- dummy(anes_postvoter$subbucketsfac, sep="", fun= as.numeric, drop=FALSE)

weightspost <- anes_postvoter$V080102

countspost <- colSums(dummiespost)

dumweightpost <- sapply(1:ncol(dummiespost),function(x) dummiespost[,x] * weightspost )
sumspost <- colSums(dumweightpost)
totalpost <- sum(sumspost)

range <- c("-100:-91", "-90:-81", "-80:-71", " -70:-61", "-60:-51", "-50:-41", "-40:-3","-30:-21", "-20:-16", "15:-11", "-10:-6", "-5:-1", "0", "1:5",  "6:10", "11:15" ,"16:20", "21:30", "31:40", "41:50",  "51:60", "61:70", "71:80","81:90", "91:100", "NA")

wpctpost <- (sumspost/totalpost)*100
percentpost <- round(wpctpost, digits=2)
names(percentpost) <- range

mayer1post <- cbind(range, countspost, percentpost)

stargazer(mayer1post, rownames=FALSE)


