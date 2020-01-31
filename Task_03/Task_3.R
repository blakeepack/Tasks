install.packages("paleobioDB", dep=T)
setwd('~/Desktop/Evolution/Tasks/Task_03')
library(paleobioDB)
Taxon <- "Dinosauria"
MinMA <- 66
MaxMA <- 252
fossils <- pbdb_occurrences(base_name = Taxon, show = c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
#how many species are known from each time period, I'm unsure if this was a question that was to be answered or something used to help guide us in teh next step splitting the species up, I believe it to be the latter but let me know if I'm mistaken.


Res <- 5
nspeciesOverTime <- pbdb_richness(fossils, rank = "genus", temporal_extent = c(MaxMA, MinMA), res=Res)
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)),nspeciesOverTime[ ,2],xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main= Taxon)

#get appearnce of data
newspeciesOverTime <- pbdb_orig_ext(fossils, res=5, rank="species",temporal_extent=c(MinMA, MaxMA))

#set up the plot
par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5, 0.5,0))

#plot first apperarnce 
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[ , 1], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab="num. of species", main=Taxon)
#add a line for the last apperance
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(newspeciesOverTime)), newspeciesOverTime[,2], col='red')
#add a legend
legend("topleft", legend=c("first appear", "go extinct"), col=c('black', 'red'), lty=1, bty="n")
#going extinct seems to preceed a rise in first appearance, this makes sense as there are new niches to be filled after an extinction
#It surprises me the rate at first appeared on the last two peaks, its far beyond anything seen in the last 200million years easily
#the dinosaurs were evolving the fastest around 60-70ma right afer a large exctinction event 

# Lets map it so we can see across space instead of time first byt changing the colors
OceanCol <- "light blue"
LandCol <- "black"
Cols <- c('#fee5d9', '#fcae91', '#fb6a4a', '#de2d26', '#a50f15')

#make a map
par(las=0)
pbdb_map_richness(fossils, col.ocean=OceanCol, col.int= LandCol, col.rich=Cols)

# new section
#first get all the Triassic fossils
MinMA<- 201
MaxMA<- 252
triassic_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#then Jurrasic
MinMA <- 145
MaxMA <- 201
jurassic_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
#then Cretacious
MinMA <-66
MaxMA <- 145
cretaceous_fossils <- pbdb_occurrences(base_name = Taxon, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)

#now make a series of maps
dev.new(height= 7.8, width=13)
pbdb_map_richness(triassic_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Triassic (252 - 201Ma)", cex=3, line=-2)

dev.new(height= 7.8, width=13)
pbdb_map_richness(jurassic_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Jurassic (201 -145Ma)", cex=3, line=-2)

dev.new(height= 7.8, width=13)
pbdb_map_richness(cretaceous_fossils, col.ocean=OceanCol, col.int =LandCol, col.rich=Cols)
mtext(side=3, "Cretaceous (145 - 66Ma)", cex=3, line=-2)

#New Section
Taxon2 <- "Mammalia"
MinMA <- 66
MaxMa <- 252
fossils2 <- pbdb_occurrences(base_name = Taxon2, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank= "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

#Plot the groups

par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_dino <- Cols[length(Cols)]
Col_mammal <- Cols[1]
LineWidth <- 2

plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab= "num. of species", col=Col_dino, lwd=LineWidth)

lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col= Col_mammal, lwd=LineWidth)

legend("topleft", legend=c(Taxon, Taxon2), col=c(Col_dino, Col_mammal), bty="n", lwd=LineWidth)

#Part8 our own stuff
Taxon3 <- "Echinodermata"
MinMA <- 85
MaxMA <- 160
fossils2 <- pbdb_occurrences(base_name = Taxon3, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank= "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

Taxon4 <- "Chordata"
fossils2 <- pbdb_occurrences(base_name = Taxon3, show=c("phylo", "coords", "ident"), min_ma=MinMA, max_ma=MaxMA)
nspeciesOverTime2 <- pbdb_richness(fossils2, rank= "genus", temporal_extent = c(MaxMA,MinMA), res=Res)

par(mar=c(4,5,2,1), las=1, tck=-0.01, mgp=c(2.5,0.5,0))
Col_chordata <- Cols[length(Cols)]
Col_echino <- Cols[1]
LineWidth <- 2
plot(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime)), nspeciesOverTime[,2], xlim=c(MaxMA, MinMA), type="l", xlab="age (millions of years ago)", ylab= "num. of species", col=Col_dino, lwd=LineWidth)
lines(seq(to=MaxMA, from=MinMA, length.out=nrow(nspeciesOverTime2)), nspeciesOverTime2[,2], col= Col_mammal, lwd=LineWidth)
legend("topleft", legend=c(Taxon3, Taxon4), col=c(Col_chordata, Col_echino), bty="n", lwd=LineWidth)
#I hypothesize that chordata and echinoderms species numbers are negatively correlated throughout this time period 





