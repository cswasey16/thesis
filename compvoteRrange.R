#plot comparitive percents vote R over -15 to 20ish range

#input mayer
mayerRorig <-  c(0, 1, 0, 1, 1, 3, 5, 6, 9, 15, 16, 35, 47, 81, 81, 85, 91, 94, 96, 96, 99, 98, 98, 99, 99, NA)

mayerDorig <- c(100, 99, 100, 99, 99, 97, 95, 94, 91, 85, 84, 65, 53, 19, 19, 15, 9, 6, 4, 4, 1, 2, 2, 1, 1, NA)

mayerpopperc <- c(2.0, 2.8, 0.4, 3.8, 4.7, 5.4, 5.3, 7.1, 4.3, 2.6, 4.5, 0.3, 8.8, 0.3, 4.7, 2.3, 3.9, 7.2, 5.8, 5.5, 5.6, 5.1, 0.6, 3.9, 3.1, NA)

mayerdata <- cbind(range, mayerpopperc, mayerDorig, mayerRorig)

mayerdata
#subtract mayer and mine

Rdiff <- mayerRorig - percentR
Ddiff <- mayerDorig - percentD
absDdiff <- abs(Ddiff)
absRdiff <- abs(Rdiff)
bucketnum <- c(1:26)
plot(bucketnum, mayerRorig)
plot(bucketnum, mayerDorig)

plot(bucketnum, percentR)
plot(bucketnum, percentD)

plot(bucketnum, absRdiff)
plot(bucketnum, absDdiff)

mayerdatadiffs <- cbind(range, mayerpopperc, mayerDorig, Ddiff, mayerRorig, Rdiff)
mayerdatadiffs

rangeref <- cbind(bucketnum, range)
rangeref
