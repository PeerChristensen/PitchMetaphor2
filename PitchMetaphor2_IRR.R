# PITCH METAPHOR STUDY 2: INTERCODER RELIABILITY

# PEER CHRISTENSEN
# OCTOBER 2017

###
library(irr)
library(psych)
library(xtable)
library(tidyverse)

#load and clean data
#df= readcsv2("IRR_data_study2.csv",sep=",",na.strings = c(""),stringsAsFactors = T)
df = read_csv("IRR_data_study2.csv") 
df$Language=ifelse(grepl("s",df$File)==T,"Swedish","Turkish") # create language column
df$File=gsub(".*bi|_.*", "", df$File)                 # shorten filename
df = df %>%                                   
  select(-X8) %>%                                     # remove empty col
  filter(!is.na(DimensionIL) & !is.na(Dimension)) %>% # remove NA rows
  rename(Time = `Begin Time - hh:mm:ss.ms`) %>%       # rename time col (new name = old name)
  mutate(Handshape=factor(Handshape),HandshapeIL=factor(HandshapeIL), # convert to factors
         Dimension=factor(Dimension),DimensionIL=factor(DimensionIL))

df$Handshape=recode(df$Handshape, "narow grip"="grip","narrow grip"="grip",
                                     "neutral grip"="grip","wide grip"="grip",
                                     "none"="other","open"="other")
df$HandshapeIL=recode(df$HandshapeIL, "narow grip"="grip","narrow grip"="grip",
                                         "neutral grip"="grip","wide grip"="grip",
                                         "none"="other","open"="other")

dfS=df[df$Language=="Swedish",]
dfT=df[df$Language=="Turkish",]

#DIMENSION - Total
select(df,DimensionIL,Dimension) %>% kappa2

#HANDSHAPE - Total
select(df,HandshapeIL,Handshape) %>% kappa2

#DIMENSION - Swedish
select(dfS,DimensionIL,Dimension) %>% kappa2

#HANDSHAPE - Swedish
select(dfS,HandshapeIL,Handshape) %>% kappa2

#DIMENSION - Turkish
select(dfT,DimensionIL,Dimension) %>% kappa2

#HANDSHAPE - Turkish
select(dfT,HandshapeIL,Handshape) %>% kappa2


#Table using kappa2 from "irr" package
CohenKappa=data.frame(Group=c("Total","Swedish","Turkish"),
                      Dimension=round(c(kappa2(df[,c(3,4)])$value,
                                        kappa2(dfS[,c(3,4)])$value,
                                        kappa2(dfT[,c(3,4)])$value),3),
                      Handshape=round(c(kappa2(df[,c(5,6)])$value,
                                        kappa2(dfS[,c(5,6)])$value,
                                        kappa2(dfT[,c(5,6)])$value),3))
CohenKappa

#Table using cohen.kappa from "psych" package with confidence intervals
KappaConfint=t(data.frame("Dimension total"=cohen.kappa(cbind(df[,c(3,4)]))$confid[1,],
                          "Handshape total"=cohen.kappa(cbind(df[,c(5,6)]))$confid[1,],
                          "Dimension Swedish"=cohen.kappa(cbind(dfS[,c(3,4)]))$confid[1,],
                          "Dimension Turkish"=cohen.kappa(cbind(dfT[,c(3,4)]))$confid[1,],
                          "Handshape Swedish"=cohen.kappa(cbind(dfS[,c(5,6)]))$confid[1,],
                          "Handshape Turkish"=cohen.kappa(cbind(dfT[,c(5,6)]))$confid[1,]))

percent=c(100-nrow(df[df$Dimension!=df$DimensionIL,])/nrow(df)*100,
          100-nrow(df[df$Handshape!=df$HandshapeIL,])/nrow(df)*100,
          100-nrow(dfS[dfS$Dimension!=dfS$DimensionIL,])/nrow(dfS)*100,
          100-nrow(dfT[dfT$Dimension!=dfT$DimensionIL,])/nrow(dfT)*100,
          100-nrow(dfS[dfS$Handshape!=dfS$HandshapeIL,])/nrow(dfS)*100,
          100-nrow(dfT[dfT$Handshape!=dfT$HandshapeIL,])/nrow(dfT)*100)

KappaConfint=round(data.frame(KappaConfint, percent),3)
KappaConfint

#How to report: 
#e.g. "There was excellent agreement between coders, κ = .593 (95% CI, .300 to .886), p < .0005."


# data with disagreement
dfDis=df[df$Dimension!=df$DimensionIL | df$Handshape!=df$HandshapeIL,]
#dfDis=dfDis[,-c(4:6,13)]
#dfDis=subset(dfDis,!is.na(dfDis$File))
DisTab<-xtable(dfDis)
print.xtable(DisTab, type="html", file="DisTab")
