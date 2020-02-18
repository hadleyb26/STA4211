#10/11
cashoffers <- paste("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2019%20Data%20Sets/CH19PR10.txt",sep = "")
cashoffersdata <- read.table(cashoffers, header = FALSE, col.names = c("offer", "agegroup", "gender", "rep"))
library(plyr)
meanoffers <- data.frame(ddply(cashoffersdata, c("agegroup", "gender"), summarise, mean=mean(offer)))
meanoffers
resids11 <- (cashoffersdata[1:6, 1] - meanoffers[1, 3])
resids12 <- (cashoffersdata[7:12, 1] - meanoffers[2, 3])
resids21 <- (cashoffersdata[13:18, 1] - meanoffers[3, 3])
resids22 <- (cashoffersdata[19:24, 1] - meanoffers[4, 3])
resids31 <- (cashoffersdata[25:30, 1] - meanoffers[5, 3])
resids32 <- (cashoffersdata[31:36, 1] - meanoffers[6, 3])
residuals <- c(resids11, resids12, resids21, resids22, resids31, resids32)
cashmod1 <- aov(offer ~ factor(agegroup) + factor(gender), data = cashoffersdata)
summary(cashmod1)
plot(residuals~fitted.values(cashmod1), main = "Fitted Values vs Residuals", xlab = expression(hat(Y)), ylab ="Residual Value")
abline(h=0)
qqnorm(residuals, datax=TRUE)
qqline(residuals)
StdErr <- summary.lm(cashmod1)$sigma
n <- 36
ExpVals <- sapply(1:n, function(k) StdErr*qnorm((k-.375)/(n+.25)))
cor(ExpVals, sort(residuals))
plot(residuals, type = "o", main="Independence", xlab = "Order", ylab = "Residual")
interaction.plot(cashoffersdata$agegroup, cashoffersdata$gender, cashoffersdata$offer)
cashmod2 <- aov(offer ~ factor(agegroup)*factor(gender), data=cashoffersdata)
summary(cashmod2)
summary.lm(cashmod2)
qf(.95, 2, 30)
qf(.95, 2, 32)
qf(.95, 1, 32)

#12/13
eyecontact <- paste("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2019%20Data%20Sets/CH19PR12.txt", sep = "")
eyecontactdata <- read.table(eyecontact, header = FALSE, col.names = c("Success", "EyeContact", "Gender", "Rep"))
library(plyr)
meansuccess <- data.frame(ddply(eyecontactdata, c("EyeContact", "Gender"), summarise, mean=mean(Success)))
meansuccess
resids11 <- (eyecontactdata[1:5, 1] - meansuccess[1, 3])
resids12 <- (eyecontactdata[6:10, 1] - meansuccess[2, 3])
resids21 <- (eyecontactdata[11:15, 1] - meansuccess[3, 3])
resids22 <- (eyecontactdata[16:20, 1] - meansuccess[4, 3])
residuals <- c(resids11, resids12, resids21, resids22)
sum(resids22)
successmod1 <- aov(Success ~ factor(EyeContact) + factor(Gender), data = eyecontactdata)
plot(residuals~fitted.values(successmod1), main = "Fitted Values vs. Residuals", xlab = "Fitted Values", ylab = "Residuals")
qqnorm(residuals)
qqline(residuals)
StdErr <- summary.lm(successmod1)$sigma
n <- 20
ExpVals <- sapply(1:n, function(k) StdErr*qnorm((k-.375)/(n+.25)))
cor(ExpVals, sort(residuals))
plot(residuals, type = "o", main = "Independence", xlab = "Order", ylab = "Residuals")
interaction.plot(eyecontactdata$EyeContact, eyecontactdata$Gender, eyecontactdata$Success)
successmod2 <- aov(Success~factor(EyeContact)*factor(Gender), data = eyecontactdata)
summary(successmod2)
qf(.99, 1, 16)

#14/15
hayfever <- paste("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2019%20Data%20Sets/CH19PR14.txt", sep ="")
hayfeverdata <- read.table(hayfever, header = FALSE, col.names = c("relief", "A", "B", "rep"))
library(plyr)
meanrelief <- data.frame(ddply(hayfeverdata, c("A", "B"), summarise, mean=mean(relief)))
meanrelief
resids11 <- (hayfeverdata[1:4, 1] - meanrelief[1, 3])
resids12 <- (hayfeverdata[5:8, 1] - meanrelief[2, 3])
resids13 <- (hayfeverdata[9:12, 1] - meanrelief[3, 3])
resids21 <- (hayfeverdata[13:16, 1] - meanrelief[4, 3])
resids22 <- (hayfeverdata[17:20, 1] - meanrelief[5, 3])
resids23 <- (hayfeverdata[21:24, 1] - meanrelief[6, 3])
resids31 <- (hayfeverdata[25:28, 1] - meanrelief[7, 3])
resids32 <- (hayfeverdata[29:32, 1] - meanrelief[8, 3])
resids33 <- (hayfeverdata[33:36, 1] - meanrelief[9, 3])
residuals <- c(resids11, resids12, resids13, resids21, resids22, resids23, resids31, resids32, resids33)
hayfevermod1 <- aov(relief~factor(A)+factor(B), data = hayfeverdata)
summary(hayfevermod1)
plot(residuals~fitted.values(hayfevermod1), main = "Fitted Values vs. Residuals", xlab = "Fitted Values", ylab = "Residual")
qqnorm(residuals)
qqline(residuals)
StdErr <- summary.lm(hayfevermod1)$sigma
n <- 36
ExpVals <- sapply(1:n, function(k) StdErr*qnorm((k-.375)/(n+.25)))
cor(ExpVals, sort(residuals))
interaction.plot(hayfeverdata$A, hayfeverdata$B, hayfeverdata$relief)
hayfevermod2 <- aov(relief~factor(A)*factor(B), data=hayfeverdata)
summary(hayfevermod2)
qf(.95, 4, 27)
qf(.95, 2, 27)

#16/17
diskdrive <- paste("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2019%20Data%20Sets/CH19PR16.txt", sep = "")
diskdrivedata <- read.table(diskdrive, header = FALSE, col.names = c("time", "technician", "make", "rep"))
library(plyr)
diskdrivemeans <- data.frame(ddply(diskdrivedata, c("technician","make"), summarise, mean=mean(time)))
diskdrivemeans
diskdrivemod1 <- aov(time~factor(technician)+factor(make), data=diskdrivedata)
diskdrivemod1
resids11 <- (diskdrivedata[1:5, 1] - diskdrivemeans[1, 3])
resids12 <- (diskdrivedata[6:10, 1] - diskdrivemeans[2, 3])
resids13 <- (diskdrivedata[11:15, 1] - diskdrivemeans[3, 3])
resids21 <- (diskdrivedata[16:20, 1] - diskdrivemeans[4, 3])
resids22 <- (diskdrivedata[21:25, 1] - diskdrivemeans[5, 3])
resids23 <- (diskdrivedata[26:30, 1] - diskdrivemeans[6, 3])
resids31 <- (diskdrivedata[31:35, 1] - diskdrivemeans[7, 3])
resids32 <- (diskdrivedata[36:40, 1] - diskdrivemeans[8, 3])
resids33 <- (diskdrivedata[41:45, 1] - diskdrivemeans[9, 3])
residuals <- c(resids11, resids12, resids13, resids21, resids22, resids23, resids31, resids32, resids33)
plot(residuals~fitted.values(diskdrivemod1), main = "Fitted Values vs. Residuals", xlab = "Fitted Values", ylab = "Residuals")
qqnorm(residuals)
qqline(residuals)
StdErr <- summary.lm(diskdrivemod1)$sigma
n <- 45
ExpVals <- sapply(1:n, function(k) StdErr*qnorm((k-.375)/(n+.25)))
cor(ExpVals, sort(residuals))
plot(residuals, type = "o")
interaction.plot(diskdrivedata$technician, diskdrivedata$make, diskdrivedata$time)
diskdrivemod2 <- aov(time~factor(make)*factor(technician), data = diskdrivedata)
summary(diskdrivemod2)
qf(.99, 4, 36)
qf(.99, 2, 36)

#18/19
kidneyfailure <- paste("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2019%20Data%20Sets/CH19PR18.txt", sep = "")
kidneyfailuredata <- read.table(kidneyfailure, header = FALSE, col.names = c("Days", "Duration", "WeightGain", "Rep"))
daystransformed <- (log10(kidneyfailuredata[ ,1] + 1))
data.frame(daystransformed)
kidneyfailuredata <- data.frame(kidneyfailuredata[ ,1], daystransformed, kidneyfailuredata[ ,2:4])
names(kidneyfailuredata)[1] <- "Days"
names(kidneyfailuredata)[2] <- "Days Transformed"
interaction.plot(kidneyfailuredata$WeightGain, kidneyfailuredata$Duration, kidneyfailuredata$`Days Transformed`)
kidneydatamod1 <- aov(`Days Transformed`~factor(Duration)*factor(WeightGain), data = kidneyfailuredata)
summary(kidneydatamod1)
qf(.95, 2, 54)


#random code
library(ALSM)
install.packages("ALSM")
aligned.dot.plot2(cashoffersdata$offer, cashoffersdata$agegroup, cashoffersdata$gender)
