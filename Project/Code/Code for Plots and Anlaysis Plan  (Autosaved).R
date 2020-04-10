# DONT READ THIS
#Data <- read.csv("http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR93_Aug2008.txt")
#read.table("http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR93_Aug2008.txt", header=T, row.num=NULL, sep="/t"

#Analysis plan checklist
#Make a basic graph for the thing due today
#Add some more sounds form the Delphinidae group, those with sounds that are easier to distinguish.
#Make a scatterplot of the Avg of Avgs of sounds versus the Avg Mass 
#Make a scatterplot of the AvgdB versus the Mass with a trendline
#Make a scatterplot of the maximums of known scientific ranges against their avg Masses
#do a pearson correlation for all of the above and compare the numbers to the expexted maximum 
#Explain the results as best i can


#read from here on

#new stuff, read from here on aslo i thought you should know i conglomerated a lot of data into one csv so i only read from one source but that because i spent like 40-60hrs on it so that everthing was in one spot. My internet going in and out this just seemed like the smartest move. 
setwd("~/Desktop/Evolution/Tasks/Project")
Data <- read.csv("~/Desktop/Evolution/Tasks/Project/Data/Whale Sound Data.csv",stringsAsFactors=F)

#Average dB graphing and tests
plot(Data$AvgMass,Data$AvgdB, xlab="Average Mass(kg)", ylab= "Average dB of Call(dB)", type="p",pch=16)
abline(lm(Data$AvgdB~ Data$AvgMass))
cor.test(Data$)
plot(Data$AvgMassR,Data$EstMax)
abline(lm(Data$EstMax~Data$AvgMassR))
cor.test(Data$EstMax,Data$AvgMassR)
lm(Data$AvgMass~Data$AvgdB)


#Here are my notes and questions I need help with. Im wanting to make a scatter plot of all the noises connected the weight in order from lowest weight to highest weigh, I know that is not what I have turned into you but I cant get the x-axis to go in order for some reason and I plan to make several graphs so this is a problem im really needing help on. My idea for doing this is converting each group into its own set of objects and somehow adding that to a large already amde graph. that way i can make each group its own color and look at them speerately


#Also i tried really hard to get as many sounds as i could but the website hosting a lot of the sounds is down and there are very few places on the internet for some of the species ive found for the specific calls im doing as they are andangered. Im going to add a few other families in there to help this out though it just takes a very long time to get the sounds, listen to them for the right calls, and thenanalyze them.

#from here on is me experimenting making my other graphs, I really need help making these scatterplots. I've spent like 5 hours trying to make it into them and I can't make any progress at all, ive watched videos but they aren't doing what i need them to do. that is why the one I turned in is out of order and is ugly, I plan on not using that one in the final one at all and instead a set of scatterplots with trendlines.

#Dont read this is experimentation
BelugaMass <- Data[1:8,36]
BelugaAvgAvg <- Data[1:8,34]
BelugaAvg <- Data[1:8,32]
BelugaMax <- Data[1:8,33]

Data$AvgMass<- as.numeric(gsub(",","",Data$AvgMass))
plot(Data$AvgMass,Data$AvgAvg,type="p", xlab="Mass kg", ylab= "Average dB sound of call")
plot(AvgMass,AvgAvg)
?plot

colnames(data)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(Data$AvgMass, Data$AvgdB, )
plot(Data$AvgMass, Data$AvgAvg)

par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(Data$AvgMass)), Data$AvgAvg, type="b", pch=16, xlab=" Mass kg", ylab="Sound dB")
plot(Data$AvgMass, Data$AvgAvg, type="b", pch=16)