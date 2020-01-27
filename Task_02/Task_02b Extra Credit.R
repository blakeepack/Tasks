getwd()
setwd("~/Desktop/Evolution/Programming/Task_02")
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


#start of bonus 
#Brandon helped a lot and so did Shane and Sarah
setwd("~/Desktop/Evolution/Tasks/Task_02")
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

Naps <- which(beren3$event == "nap")
beren4 <- beren3[Naps,]
head(beren4)

#naming stuff
beren4$end_minute <- beren4$end_minute / 60
beren4$start_minute <- beren4$start_minute / 60
beren4$napstart <- beren4$start_hour + beren4$start_minute
beren4$napend <- beren4$end_hour + beren4$end_minute
beren4$naplength <- beren4$napend - beren4$napstart

naptime <- tapply(beren4$naplength, beren4$age, sum, na.rm=T)

#graphing 
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5 ,0), tck=-.01) 
plot(as.numeric(names(naptime)), naptime, type="b", pch=16, xlab="day", ylab="naptime")
cor.test(beren4$age, beren4$naplength)

#Bonus ends here thanks for the help
























