# PITCH METAPHOR STUDY 2: LHQ DATA

# DESCRIPTION:

# Distribution of age and gender
# Self-reported speaking ability
# Contexts of language use

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
library(tidyr)
library(plyr)

lhq = read.csv2("LHQ_biling.csv",header=T,stringsAsFactors=FALSE,skip=1)

#age - mean, sd and se
mean(lhq$Age)
sd(lhq$Age)
sd(lhq$Age)/sqrt(length(lhq$Age))

#gender
table(lhq$Sex)

# speaking ability
speak=unname(lhq[,c(1,25:32)])
speak=rbind(data.frame(speak[,c(1,2:3)]),
            data.frame(speak[,c(1,4:5)]),
            data.frame(speak[,c(1,6:7)]),
            data.frame(speak[,c(1,8:9)]))
speak=speak[speak$X1!="",]
names(speak) = c("Participant","Language","Rating")

ag <- aggregate(Rating ~ Language, data=speak, function(x) 
  c(mean = round(mean(x),2), 
    sd = round(sd(x),2)))

#with ddply + include n

aggSpeak = ddply(speak, "Language", summarize,
           meanRating = round(mean(Rating), 2),
           sd = round(sd(Rating), 2),
           n = length(Rating))
aggSpeak=aggSpeak[-1,]
aggSpeak$sd[is.na(aggSpeak$sd)] = 0
aggSpeak$se=round(aggSpeak$sd/sqrt(length(aggSpeak$n)),2)
aggSpeak

# language use in different contexts
use=lhq[,c(1,33:36)]
home=table(use$At.home..Speaking)
home
friends=table(use$With.friends..Speaking)
friends
occupation=table(unite(use, "occupation", c("At.school..Speaking","At.work..Speaking"),sep="")$occupation)
occupation


