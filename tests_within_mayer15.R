#testing for overall number of mayer swing voters


anes_2008$mayer15 <- ifelse(anes_2008$cand_therm_sub < 16 & anes_2008$cand_therm_sub > -16, TRUE, FALSE)
mayer15 <- subset(anes_2008, anes_2008$mayer15 == TRUE)

#587/2322 = .2527993

anes_2008$V083037a   #D therm

#original scale classifications

mayer15$under50 <- ifelse(mayer15$V083037a < 50 & mayer15$V083037b < 50, TRUE, FALSE)
mayer15$over50 <- ifelse(mayer15$V083037a > 50 & mayer15$V083037b > 50, TRUE, FALSE)

mayer15$cand_therm_sub_abs <- abs(mayer15$cand_therm_sub)
anes_2008$cand_therm_sub_abs <- abs(anes_2008$cand_therm_sub)
