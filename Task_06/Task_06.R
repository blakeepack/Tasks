# Sarah Daniel, Brandon Keaton, Nina Patelli, and Shane Harless worked with me 

setwd('~/Desktop/Evolution/Tasks/Task_06')

#installing stuff
library("learnPopGen")
install.packages("coala")
install.packages("phytools")
library("coala")
install.packages("ape")
install.packages("maps")
library("phytools")

#part 1 
#Question 1 
#How many alleles, was 3, 12, and 100. And you can change it by editing n in the coalescent plot
#Question 2 
#It definitely has a correlation to the size of the starting effective population, the higher that starting effective population the longer it takes for them to go to fixation. I'm thinking this is correlated to the 4Ne that we talked about in class but I'm not sure, thought. In the ones I looked at they seemed to be around 20 but in the one that was 100 effectiveley in the beginning. 
#Question 3 
#One offspring per individual seemed to be the average from what we said
#Question 4 
#No role at all 
#Question 5
#No they're not most of the time in the several other ones I ran they were from a bottlneck of some sort


coalescent.plot()
coalescent.plot(n=10, ngen=50, colors=NULL)
#First simulation 
model<- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
feat_mutation(10)+
feat_recombination(10)+
sumstat_trees()+
sumstat_nucleotide_div()

stats <- simulate(model, nsim=1)

Diversity<- stats$pi
head(Diversity)
#all the numbers are different, the difference is due to the differnt mutation and recombinations I think

Nloci <- length(stats$trees)

t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#Question 6 
#Because it is dipolidy so it is double what it should be 
Agel<- max(nodeHeights(t1))

t2<- read.tree(text=stats$trees[[2]][1])

plot(t2)
axisPhylo()
#Question 7 
#No they don't match. 



par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()

compare.chronograms(t1,t2)

t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)

for(locus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for(n in 1:ntrees) { 
		if(locus==1&& n==1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
			outPhy <- ape:::c.phylo( outPhy, read.tree(text=stats$trees[[locus]][n]))
			}
			}
			}

	

par(mfrow=c(1,1))
densityTree(outPhy)


model3 <-coal_model(10, 50)+
	feat_mutation(par_prior("theta", sample.int(100, 1)))+
	sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)	

mean_pi<- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])

#plotting
plot(mean_pi,theta)
abline(lm(mean_pi ~ theta))
