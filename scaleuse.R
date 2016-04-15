#use of scale investigation
library(plyr)

Rthermscores <- count(anes_2008, vars= "V083037b", wt_var = "V080102")
Dthermscores <- count(anes_2008, vars= "V083037a", wt_var = "V080102")
thermscores <- cbind(Dthermscores, Rthermscores)
plot(Rthermscores)
plot(Dthermscores)


Rthermpcts <- wpct(anes_2008$V083037b, weight= anes_2008$V080102, na.rm=TRUE)*100
Rthermpcts
100-sum(Rthermpcts["0"], Rthermpcts["15"], Rthermpcts["30"], Rthermpcts["40"], Rthermpcts["50"], Rthermpcts["60"], Rthermpcts["70"], Rthermpcts["85"], Rthermpcts["100"])

Dthermpcts <- wpct(anes_2008$V083037a, weight= anes_2008$V080102, na.rm=TRUE)*100
Dthermpcts
100-sum(Dthermpcts["0"], Dthermpcts["15"], Dthermpcts["30"], Dthermpcts["40"], Dthermpcts["50"], Dthermpcts["60"], Dthermpcts["70"], Dthermpcts["85"], Dthermpcts["100"])


mainpoints <- c(0, 15, 30, 40, 50, 60, 70, 85, 100, "Other")
Dmainpointpct <- round(c(Rthermpcts["0"], Rthermpcts["15"], Rthermpcts["30"], Rthermpcts["40"], Rthermpcts["50"], Rthermpcts["60"], Rthermpcts["70"], Rthermpcts["85"], Rthermpcts["100"], 2.8), digits=1)
Rmainpointpct <- round(c(Dthermpcts["0"], Dthermpcts["15"], Dthermpcts["30"], Dthermpcts["40"], Dthermpcts["50"], Dthermpcts["60"], Dthermpcts["70"], Dthermpcts["85"], Dthermpcts["100"], 3.7), digits=1)
mainpointtable <- cbind(Dmainpointpct, Rmainpointpct)
stargazer(mainpointtable)

