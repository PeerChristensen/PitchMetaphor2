# PITCH METAPHOR STUDY 2: SPEECH

# DESCRIPTION:


#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------

### LOAD DATA AND PACKAGES, OMIT VARIABLES
library(ggplot2)
library(plyr)
library(Hmisc)
library(reshape)

df = read.csv2("PitchMetaphor2_CLEAN_edit.csv",header=T,stringsAsFactors=FALSE)
df = df[,-c(1,2,4,11:14)]

#-------------------------------------------------------------------------------
### NUMBER OF OBSERVATIONS

numObs = nrow(df)
# By language
ObsByLang = table(df$Language)

#-------------------------------------------------------------------------------
### METAPHORS

#Overview of all metaphors
allMetaphors = table(df$Metaphor,df$Language)

# Categorise metaphors with < 10 obs. as "Other"
df$Metaphor[df$Metaphor=="?" | df$Metaphor=="Mildness" | df$Metaphor=="Strength" |
              df$Metaphor=="Openness" | df$Metaphor=="Roughness" |
              df$Metaphor=="Roundness" | df$Metaphor=="Sharpness"] = "Other"

# By language
#Swedish
sweDat=df[df$Language=="Swedish",]
sweDat=ftable(sweDat$Participant,sweDat$Metaphor)
wtS=rowSums(sweDat)
sweDat=round(prop.table(as.matrix(sweDat),1),2)
sweDat=data.frame(sweDat,wtS)
sweDat$Language="Swedish"

sweDat=ddply(sweDat,"Language",summarise,
          H= weighted.mean(Height,wtS), 
          B=weighted.mean(Brightness,wtS),
          M=weighted.mean(Mass,wtS),
          S=weighted.mean(Softness,wtS),
          Th=weighted.mean(Thickness,wtS),
          O=weighted.mean(Other,wtS),
          Hse=sqrt(wtd.var(Height,wtS))/sqrt(nrow(sweDat)),
          Bse=sqrt(wtd.var(Brightness,wtS))/sqrt(nrow(sweDat)),
          Mse=sqrt(wtd.var(Mass,wtS))/sqrt(nrow(sweDat)),
          Sse=sqrt(wtd.var(Softness,wtS))/sqrt(nrow(sweDat)),
          Thse=sqrt(wtd.var(Thickness,wtS))/sqrt(nrow(sweDat)),
          Ose=sqrt(wtd.var(Other,wtS))/sqrt(nrow(sweDat)))

sweDat <- transform(sweDat, Hlower=H-Hse, Hupper=H+Hse,Blower=B-Bse,
                 Bupper=B+Bse,Mlower=M-Mse,Mupper=M+Mse,Slower=S-Sse,Supper=S+Sse,
                 Thlower=Th-Thse,Thupper=Th+Thse,Olower=O-Ose,Oupper=O+Ose)

sweDat=melt(sweDat,Language=Language)

sweDat=data.frame(cbind(sweDat[1:6,],
                     sweDat[7:12,2:3]))
sweDat[,c(3,5)]=round(sweDat[,c(3,5)],3)

BarSwe <- ggplot(sweDat, aes(x=variable, y=value, fill=Language)) + 
  geom_bar(position=position_dodge(),stat="identity",fill="#21908CFF" ,colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
 # geom_text(aes(label = value, y = 0.3, size = 3)) +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("H","B","M","S","Th","O"),
                   labels=c("Height", "Brightness","Mass","Softness","Thickness","Other")) +
  xlab("Metaphors") +
  ggtitle("Swedish speech metaphors") +
  theme_classic() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
BarSwe

#Turkish
turDat=df[df$Language=="Turkish",]
turDat$Metaphor[turDat$Metaphor=="Mass" | turDat$Metaphor=="Softness"] = "Other"
turDat=ftable(turDat$Participant,turDat$Metaphor)
wtS=rowSums(turDat)
turDat=round(prop.table(as.matrix(turDat),1),2)
turDat=data.frame(turDat,wtS)
turDat$Language="Turkish"

turDat=ddply(turDat,"Language",summarise,
             H= weighted.mean(Height,wtS), 
             B=weighted.mean(Brightness,wtS),
             Th=weighted.mean(Thickness,wtS),
             O=weighted.mean(Other,wtS),
             Hse=sqrt(wtd.var(Height,wtS))/sqrt(nrow(turDat)),
             Bse=sqrt(wtd.var(Brightness,wtS))/sqrt(nrow(turDat)),
             Thse=sqrt(wtd.var(Thickness,wtS))/sqrt(nrow(turDat)),
             Ose=sqrt(wtd.var(Other,wtS))/sqrt(nrow(turDat)))

turDat <- transform(turDat, Hlower=H-Hse, Hupper=H+Hse,Blower=B-Bse,
                    Bupper=B+Bse,Thlower=Th-Thse,Thupper=Th+Thse,Olower=O-Ose,
                    Oupper=O+Ose)

turDat=melt(turDat,Language=Language)

turDat=data.frame(cbind(turDat[1:4,],
                        turDat[5:8,2:3]))
turDat[,c(3,5)]=round(turDat[,c(3,5)],3)

Bartur <- ggplot(turDat, aes(x=variable, y=value, fill=Language)) + 
  geom_bar(position=position_dodge(),stat="identity",fill="#21908CFF" ,colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  # geom_text(aes(label = value, y = 0.3, size = 3)) +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("H","B","Th","O"),
                   labels=c("Height", "Brightness","Thickness","Other")) +
  xlab("Metaphors") +
  ggtitle("Turkish speech metaphors") +
  theme_classic() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
Bartur

### WORDS

# By metaphor

# By lemma

#-------------------------------------------------------------------------------

### LEMMATA