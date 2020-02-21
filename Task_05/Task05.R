setwd('~/Desktop/Evolution/Tasks/Task_05')
source("http://jonsmitchell.com/code/fxn05.R")

Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h=1, s=0)

plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type="l",xlab="generation", ylab= "allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,1], lwd=2, col='red' )
legend("topleft", legend=c ("a", "b"), col=c("black", "red"), lwd=2, bty="n")
#changing these to study 
plotFit(nruns=50, n=20, ngens=100, init_p=0.5, h=1.5, s=0.1)

Expectation <- c(10, 10, 10, 10)
Observed <- c(15, 15, 5, 5)
Chisq <- sum(((Expectation - Observed)^2)/2)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2~"="~.(Chisq)), legend.text=c("expected", "observed"))
#The Chi squared values for 4 10's is 10
#The Chi squared value for one column of 40 is 92.5
#the more even the bars, the lower the chi squared values 


results<- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts<- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol<- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#The bars are very uneven 
#They become more even as it gets lower
#That higher chi values are more uneven than lower chi squared values, thats all I can really gather. 
Avg
#60.99 is the average, I would interpret this as being uneven between the plots and as for how it compares to the value in the packet it is a lot higher than the critical value.



Avg <- mean(Chisqs)
backroundAvgs <- tapply(CHisqs, results[,3], mean)

propSig<- length(which(Chisqs>11.70)/length(Chisqs))
percSig<- round(100*propSig)
propSig
percSig
#Yes honestly Im quite confused on how they had a 500%, that makes no sense to me. 
#Definitely not just natural selection, from what you've said in class natural selection doesnt work quite how we think so it's hard for me to say
#


par(las=1, mar=c(4,4,1,1), mgp= c(2, 0.5, 0), tck=-0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")

par(las=1, mar=c(4,4,1,1), mgp= c(2, 0.5, 0), tck=-0.01, cex.axis=1)
plot(1,1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")

axis(2, at= 1:length(backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)

counter<-1
for (i in backgrounds) {
	Data <- Chisqs[which(results[,3]==i)]
	addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
	counter<- counter+1
}

abline(v=11.7, lty=2, col='black')
#like a little bit but it is hard to tell based on the size. But they all tend to be mostly around the 0-100ish range

#Running a simulation

Simulation <- simDraws(10000)

addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")

abline(v=11.70, lty=2, lwd=2)

Fit<- c(1,1,1,1,1,1)
names(Fit)<- 1:6
Simulation2<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0, 0.25))

#one selected against
Fit<- c(0.1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation3<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))

#three tooth picks types selected 
Fit<- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit)<- 1:6
Simulation4<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))

#Five selected against 
Fit<- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit)<- 1:6
Simulation5<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))

#insane selection
Fit<- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit)<- 1:6
Simulation6<- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim.")

#Simulation 7
Simulation7<- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,,0.25))
#Simulated is much more of what would be expected under random selection as there is less human induced error.
#I think yes as their chi values were largeer and less evenly spread out.
#I could be wrong but I'm thinking that there was a diretional selection for specific colors as some chi-squared values are consistently higher than others, such as blue and green. 

#Directional along with some unnecesary drift due to the nonreandomness of the human selection and  as it is predicatble what is mroe likely to survive in specific colors.
#Directional without as much drift due to the true randomness of the selection
#They they are drfiting into uneveness so the drift is stronger in their tests
#I think the critical value would matter more as it is a constant whereas the other test could be less accurate.
#I think the ability to mutate would add varaince to the population as some could be elected  for or against but still remain in the population and introduce different alleles into said popualtion. As long as this is heritable I would think it would speed up fixation as more would/could be selected for the optimal color. On the other hand ones that were already selected for could turn into non advantagious colors so there would always be an inherent varaince that could be reintroduced into a population. 

#Brandon Keaton, Sarah Daniel, and Shane Harless helped me 
