Data <- read.csv("http://jonsmitchell.com/data/beren.csv" , stringsAsFactors=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:5, 1:3]
beren<- Data
Feeds <- which(beren[,9] =="Nap")
berenNap <- beren[Feeds,]
head(berenNap)
Feeds <- which(beren[,"event"] == "Nap")
Feeds <- which(beren$event == "Nap")
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format= "%Y-%n-%d", origin = "2019-04-18")
beren$age <- dateID - dateID[which(beren$event =="birth")]
head(beren)
beren2 <- beren
beren3 <- beren2[order(beren2$age) ,]

#Here is the start of Part C I hypothesize that the rate at which beren's nap times go down is going to be greater after the intro to sleeping through the night 

# set up directory & data
setwd("~/Desktop/Evolution/Tasks/Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

# make a nap duration column
Naps <- which(beren3$event == "nap")
beren4 <- beren3[Naps,]
beren4$end_minute <- beren4$end_minute / 60
beren4$start_minute <- beren4$start_minute / 60
beren4$napstart <- beren4$start_hour + beren4$start_minute
beren4$napend <- beren4$end_hour + beren4$end_minute
beren4$naplength <- beren4$napend - beren4$napstart

# use which() to find the row that == the thing
keyDate <- beren3[grep("sleeping_thru", beren3$event),"age"]

# use which() to find which rows are < the thing
youngerDays <- which(beren4$age < keyDate)
youngerBeren <- beren4[youngerDays,]

# calculate the mean or slope or whatever you need for berenYounger
youngerSlope <- lm( naplength ~ age, data=youngerBeren)

# repeat above steps for which rows are > the thing

#Use which() to find the rows are > the thing
olderDays <- which(beren4$age > keyDate)
olderBeren <- beren4[olderDays,]
#Calculate the mean or slope of the rate 
olderSlope <- lm( naplength ~ age, data=olderBeren)

youngerSlope
olderSlope


#SLope Naming 
yS<- summary(youngerSlope)$coef[2,1:2]
oS<- summary(olderSlope)$coef[2,1:2]

#Plotting
par(mfrow=c(1,2))
boxplot(rnorm(1e3, yS[1], yS[2]), rnorm(1e3, oS[1], oS[2]))
plot(youngerBeren$age, youngerBeren$naplength, pch=16, col="red", xlim=c(100,250))
points(olderBeren$age, olderBeren$naplength, pch=16, col="blue")
abline(youngerSlope)
abline(olderSlope)
dev.off()

#The data does not support my ideas and I included both graphs and trendlines to show this in the most honest and informative way I could think of

#Bonus predict a nap length on Monday
Beren5 <-lm( naplength ~ age, data=beren4)
Beren5
zxq <- 0.002255*274
zxxq<- .438686+zxq
zxxq*60
#So the formula is .438686+0.002255*age
#Plugging in for Monday I get an 1.056556hour nap on Monday or 63.39336 minutes
#Finish here for assignment c and Bonus below here is just extra notes I was unsure where to put 















#Stuff Mitchell showed me about graphing this is not a part of any assignment and is more of a placeholder for future graphing stuff than anything do not run or grade 

boxplot(rnorm(1e3, yS[1], yS[2]), rnorm(1e3, oS[1], oS[2]))
# first option: two scatterplots
par(mfrow=c(1,2))
plot(youngerBeren$age, youngerBeren$naplength)
abline(youngerSlope)
plot(olderBeren$age, olderBeren$naplength)
abline(olderSlope)
 
plot(youngerBeren$age, youngerBeren$naplength, pch=16, col="red", xlim=c(100,250))
points(olderBeren$age, olderBeren$naplength, pch=16, col="blue")
 
par(mfrow=c(1,1))
plot(youngerBeren$age, youngerBeren$naplength, pch=16, col="red", xlim=c(100,250))
points(olderBeren$age, olderBeren$naplength, pch=16, col="blue")
 
 yS <- summary(youngerSlope)$coef[2,1:2]
 oS <- summary(olderSlope)$coef[2,1:2]
 
 boxplot(rnorm(1e3, yS[1], yS[2]), rnorm(1e3, oS[1], oS[2]))
 yS <- summary(youngerSlope)$coef[2,1:2]
  oS <- summary(olderSlope)$coef[2,1:2]
 boxplot(rnorm(1e3, yS[1], yS[2]), rnorm(1e3, oS[1], oS[2]))

