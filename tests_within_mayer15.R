#testing for overall number of mayer swing voters


anes_2008$mayer15 <- ifelse(anes_2008$cand_therm_sub < 16 & anes_2008$cand_therm_sub > -16, TRUE, FALSE)
mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)

#587/2322 = .2527993

anes_2008$V083037a   #D therm

