#interest and efficacy

summary(anes_2008$V083002)

anes_2008$thoughtaboutelection <- recode(anes_2008$V083002, " '1. Quite a lot'=1; '5. Only a little'=0; else=NA")
summary(anes_2008$thoughtaboutelection)

plot(anes_2008$thoughtaboutelection, anes_2008$cand_therm_sub)
