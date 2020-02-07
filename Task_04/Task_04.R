# Part 2 
#making our populations
setwd('~/Desktop/Evolution/Tasks/Task_04')
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)

#Now take a sample of each population
Size <- 50

Sample1 <- sample(population1, Size)

Sample2 <- sample(population2, Size)
#comparing the different samples question, they are slightly different but ranges overlap almost entriely excpet for sample 2's being slightly shorter
boxplot(Sample1, Sample2)

# Part 3

source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

#Now we make alan 
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

Focus <- makeBaby(Brenda, Alan)

#They should share half I would think
ToMom <- length(grep("mom", Focus))/length(Focus)
# I would think they would share half again as they make up the entirety of the mother who makes up half. I figured it would be 0.25 each but it is not at all like that with the grandma making up 0.4724 and the grandfather making up 0.0276. 

ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
ToDadMom <- length(grep("grandma_da", Focus))/length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/length(Focus)

#Focus is not equally related to any of the grandparents maternal or paternal with the realtionships following like this to mommom= 0.4724, momdad=0.0276, daddad=0.34605, and dadmom=0.15395. The average relatedness is 0.25 but Idk if I added them up right honestly.
Sibling_01 <- makeBaby(Brenda, Alan)
#I would imagine siblings would share 50% of their DNA with the siblings but I'm unsure of how to actually test it

ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)

ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))

#Summarize the data using quantiles
quantile(ManySiblings)
mean(ManySiblings)

#And we can also plot the data 
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#It follows with a bell curve distribution and things on the outer edge are not as common due to random recombinations or just in part due to the randomness of the creation of a new person and what will actually get passed on 

#Part 4 
HWE <- function(p)  { 
	aa <- p^2
	ab <- 2 * p * (1-p)
	bb <- (1-p)^2
	return(c(aa=aa, ab=ab, bb=bb))
	}
	HWE(0.5)
	
	#Make a blank plot
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")

#Calculate genotype frequencies 
p <- seq(from=0, to=1, by= 0.01)
GenoFreq <- t(sapply(p, HWE))

#plot our known allele frequency against our expected genotype frequencies 
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#I believe I can yes. They increase with it and decrease with it as the more a the more aa and the less a the less aa. 

#now the other genotypes
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"ab"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple","blue"), lty=1, lwd=2, bty="n")

#Lets simulate a population
Pop <- simPop(500)

#now add the points 
points(Pop[, "freqa"], Pop[, "Genotypes.aa"]/500, pch=21, bg="red")
#no they don't match, it's impossible for them to as theyre not infinite in population size and thus drift is inevitable 

#another population with a smaller population
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=22, bg="red")
# the rise in genotype frequency is much lower as freq. of allele a increases and this is definiteyl do to the new number in our simulated, the smaller the population the quicker genetic drift can occur.

#Part 5 
install.packages("learnPopGen", dep=T)
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)

PopSizes <- 5:50
Samples <- rep(PopSizes, 5)

tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))

Line <- lm(tExt ~ Samples)
summary(Line)

Line$coef

plot(Samples, tExt)
abline(Line)
#It seems to be that the points are more densely packed around it in the begining and begin to spread out much further soon after and increasingly so as samples increase. I suppose this means that our linear model isnt taking into account the variability across the range, at least thats what I found the meaning of heteroskedacitiy to mean. 

#Extra Credit 
#I couldnt figure it out, but if the bonus is still worth credit late I'll give it another shot this weekend or monday with your help, thanks for the emails though.





