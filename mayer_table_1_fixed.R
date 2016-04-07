#create weighted percents per bucket in base data

dummies <- dummy(anes_2008$subbucketsfac, sep="", fun= as.numeric)

weights <- anes_2008$V080102

counts <- colSums(dummies)

dumweight <- sapply(1:ncol(dummies),function(x) dummies[,x] * weights )
sums <- colSums(dumweight)
total <- sum(sums)

wpct <- (sums/total)*100
percent <- round(wpct, digits=2)
names(percent) <- range

range <- c("-100:-91", "-90:-81", "-80:-71", " -70:-61", "-60:-51", "-50:-41", "-40:-3","-30:-21", "-20:-16", "15:-11", "-10:-6", "-5:-1", "0", "1:5",  "6:10", "11:15" ,"16:20", "21:30", "31:40", "41:50",  "51:60", "61:70", "71:80","81:90", "91:100", "NA")


mayer1 <- cbind(labels, counts, percent)

install.packages("stargazer")
library(stargazer)

stargazer(mayer1, rownames=FALSE)

plot(percent)
