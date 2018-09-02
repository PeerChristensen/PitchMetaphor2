# PITCH METAPHOR STUDY 2: LHQ DATA

# DESCRIPTION:

# Distribution of age and gender
# Self-reported speaking ability
# Contexts of language use

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
library(tidyr)
library(plyr)
library(tidyverse)
library(magrittr)

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

# test difference in speaking ability - Swe-Tur
df_1 <- lhq %>% 
  select(Participant.ID, Language.1,Language.1..Speaking) %>% 
  rename("Lang" = Language.1, "Rating" = Language.1..Speaking)

df_2 <- lhq %>% 
  select(Participant.ID, Language.2,Language.2..Speaking) %>% 
  rename("Lang" = Language.2, "Rating" = Language.2..Speaking)

df <- full_join(df_1,df_2)

t.test(Rating~Lang,df) # n.s.

d %>% gather(.,-Participant.ID)

#########################################
# language use in different contexts

use=lhq[,c(1,33:36)]
home=table(use$At.home..Speaking)
home
friends=table(use$With.friends..Speaking)
friends
occupation=table(unite(use, "occupation", c("At.school..Speaking","At.work..Speaking"),sep="")$occupation)
occupation

# PLOT
use=lhq[,c(1,33:36)]
use %<>% as_tibble() %>%
  dplyr::rename(Home = contains("home"), Friends = contains("friends")) %>%
  unite("School/Work", c(contains("school"),contains("work")),sep="") %>%
  mutate("School/Work" = recode(`School/Work`,"SwedishSwedish"="Swedish")) %>%
  mutate(Home = fct_lump(Home,n=2), `School/Work` = fct_lump(`School/Work`)) %>%
  select(-Participant.ID)

use %>% count(Home)


  
############################################
# AoA
aoa_1 <- lhq %>% 
  select(Participant.ID, Language.1,Language.1..Years.of.use) %>% 
  dplyr::rename("Language" = Language.1, "AoA" = Language.1..Years.of.use)

aoa_2 <- lhq %>% 
  select(Participant.ID, Language.2,Language.2..Years.of.use) %>% 
  dplyr::rename("Language" = Language.2, "AoA" = Language.2..Years.of.use) %>%
  mutate(AoA = recode(AoA, `1`=0L))

df_aoa <- full_join(aoa_1,aoa_2)

t.test(AoA~Language,df_aoa) # n.s.

######
# table AoA, speaking ability
df_aoa %<>% as_tibble() %>% group_by(Language) %>% dplyr::summarize(m = mean(AoA), sd = sd(AoA))

aggSpeak %<>% filter(Language == "Swedish" | Language == "Turkish") %>% select(Language, meanRating,sd) %>%
  dplyr::rename(m = "meanRating")



