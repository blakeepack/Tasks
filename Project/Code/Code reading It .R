#This is an outline of my basic plan of attack
#Basically my plan is to make a graph of correlation between the mass from pantheria and the sound recordings I do myself from a mammalian sound database listed below this paragraph. I picked five sound clips from each species available and selected them firslty based off of being collected at different times and places to diversify pool of subjects. I then try to pick longer clips to get a more accurate average frequency and listen to them before picking them. I haven't ran it through a sound file on the computer yet and have used my phone so far but I plan to go back and fix them all.

setwd("~/Desktop/Evolution/Tasks/Project")

# Mass Data
Data <- read.csv("http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR93_Aug2008.txt")
read.table("http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR93_Aug2008.txt", header=T, row.num=NULL, sep="/t"

#Noise Data
NoiseData<- read.csv("~/Desktop/Evolution/Tasks/Project/Evolution Sound Data.csv")