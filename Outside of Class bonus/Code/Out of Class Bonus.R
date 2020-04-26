# so basically the idea behind this is that I've been working out 5 times a week since decemberish when I got a membership at a local gym. To help keep myself motivated at home and just to see how much I've grown I decided to make a graph of my bench press weights and what I was comfortable lifting. This will not be my maximums but weights I feel comfortatble lifitng multiple times. Aiming for aroun 6-12 times to be at least comfortable depending on the if it was a later set or an earlier set. 
setwd("~/Desktop/Evolution/Tasks/Outside of Class bonus")
Data<-read.csv("~/Desktop/Evolution/Tasks/Outside of Class bonus/Weight Data/Bench Weigh csv.csv")
plot(Data$Week, Data$Weight)
abline(lm(Data$Weight~Data$Week))
cor.test(Data$Weight,Data$Week)
#I did some playing around with the regression line I ahve and here are some fun facts.

#I expect this regression line to level out as weight gain has become harder for me as I've gone on, I was unhealthily skinny before working out due to my own mental problems and am just now back to my bodies healthy medium I would say. It will get harder to build form here I think but here are some encouraging and outlandish predictions.
#here are my weight goals and there corresponding week I should hit them
#135 is my own body weight and I hit that at week 14
#150 is the next goal and I should have hit it at around week 22ish so that is excitingly close
#160 somewehre around week 25
#175 around week31 whcih is a little dishearteningly further away than I hoped
#200 around week 40
#that is where I will end the realistic ones as the futher we get the less accurate I believe these are. These are just for fun becasue I'm a nerd.
#1000lbs should be possible in 325 weeks
#10,000 should be around 3,540 weeks from when I started and I will be 90 years old. A very buff 90 year old too.