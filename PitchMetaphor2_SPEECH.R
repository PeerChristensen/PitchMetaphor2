# PITCH METAPHOR STUDY 2: SPEECH

# DESCRIPTION:

# 1. Metaphors
# 2. Most frequent words grouped by Metaphor
# 3. Most frequent lemmata grouped by Metaphor

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------

### LOAD DATA AND PACKAGES, OMIT VARIABLES
library(ggplot2)
library(plyr)
library(Hmisc)
library(reshape)
library(data.table)
library(jtools)
library(viridis)
library(tidyverse)

df = read.csv2("PitchMetaphor2_CLEAN_edit_3.csv",header=T,stringsAsFactors=FALSE)
df = df[,-c(1)]
df$Participant=factor(df$Participant)
#-------------------------------------------------------------------------------
### NUMBER OF OBSERVATIONS

numObs = nrow(df)
# By language
ObsByLang = table(df$Language)

#-------------------------------------------------------------------------------
### METAPHORS BY LANGUAGE

#Overview of all metaphors
allMetaphors = table(df$Metaphor,df$Language)

# Categorise metaphors with < 10 obs. as "Other"
# Or < 3 % : prop.table(ftable(allMetaphors),2)*100
df$Metaphor[df$Metaphor=="?" | df$Metaphor=="Mildness" | df$Metaphor=="Strength" |
              df$Metaphor=="Openness" | df$Metaphor=="Roughness" | df$Metaphor=="Softness" |
              df$Metaphor=="Roundness" | df$Metaphor=="Sharpness" | df$Metaphor=="Mass"] = "Other"
df$Metaphor=factor(df$Metaphor)


#WEIGHTED
df %>%
  group_by(Language,Participant,Metaphor) %>%
  summarise (n = n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  ddply(c("Language","Metaphor"),summarise,
      Freq = weighted.mean(freq,wt),
      se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Metaphor,y=Freq,fill=Metaphor)) +
  geom_bar(stat="identity",colour="black",size=.2) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language) +
  theme_apa(legend.pos = "none") + 
  scale_fill_viridis_d(option = "D",alpha=.8,begin=.1,end=.95) +
  ggtitle("Distribution of speech metaphors") +
  ylab("Weighted mean proportions") +
  xlab("Metaphors")
  

#UNWEIGHTED   
df %>%
  group_by(Language, Participant, Metaphor) %>%
  summarise(n=n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  ddply(c("Language","Metaphor"),summarise,
        Freq = mean(freq),
        se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Metaphor,y=Freq,fill=Metaphor)) +
  geom_bar(stat="identity",colour="black",size=.2) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language) +
  theme_apa(legend.pos = "none") + 
  scale_fill_viridis_d(option = "D",alpha=.8,begin=.1,end=.95) +
  ggtitle("Distribution of speech metaphors") +
  ylab("Mean proportions")  +
  xlab("Metaphors")



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
  geom_bar(position=position_dodge(),stat="identity",colour="gray30") + # fill="#21908CFF" 
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
  geom_bar(position=position_dodge(),stat="identity" ,colour="gray30") + #add e.g. fill= "Color"
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

#-------------------------------------------------------------------------------
### WORDS BY METAPHOR

wordsMeta=df[,c(2,6,7)]
wordsMeta=data.table(na.omit(wordsMeta))

#Swedish
wordsMetaSwe=wordsMeta[wordsMeta$Language=="Swedish"]
wordsMetaSwe[,Freq := .N, by=Words]
wordsMetaSwe=unique(wordsMetaSwe)
wordsMetaSwe=wordsMetaSwe[order(-Freq)]
#wordsMetaSwe=wordsMetaSwe[1:15,] -the 15 most frequent words
wordsMetaSwe$Percent= wordsMetaSwe$Freq/328*100 # 328 given by sum(table(df$Words[df$Language=="Swedish"]))
wordsMetaSwe=wordsMetaSwe[wordsMetaSwe$Percent>=1]
wordsMetaSwe=wordsMetaSwe[wordsMetaSwe$Words!="andra"]

wmSwe=ggplot(wordsMetaSwe,aes(reorder(Words,Percent),Percent)) + 
  geom_bar(stat = "identity",aes(fill=Metaphor),colour="gray30") +
  #scale_fill_viridis("Metaphor",discrete = TRUE, option = "D") +
  #scale_fill_manual("Metaphor",values=cols_2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency in %") +
  coord_flip() +
  theme_minimal()
wmSwe

#Turkish
wordsMetaTur=wordsMeta[wordsMeta$Language=="Turkish"]
wordsMetaTur[,Freq := .N, by=Words]
wordsMetaTur=unique(wordsMetaTur)
wordsMetaTur=wordsMetaTur[order(-Freq)]
#wordsMetaTur=wordsMetaTur[1:15,] -the 15 most frequent words
wordsMetaTur$Percent= wordsMetaTur$Freq/257*100
wordsMetaTur=wordsMetaTur[wordsMetaTur$Percent>=1]

wmTur=ggplot(wordsMetaTur,aes(reorder(Words,Percent),Percent)) + 
  geom_bar(stat = "identity",aes(fill=Metaphor),colour="gray30") +
  #scale_fill_viridis("Metaphor",discrete = TRUE, option = "D") +
  #scale_fill_manual("Metaphor",values=cols_2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Turkish grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency in %") +
  coord_flip() +
  theme_minimal()
wmTur

#-------------------------------------------------------------------------------
### LEMMATA BY METAPHOR

lemMeta=df[,c(2,6,7)]
lemMeta$Lemma=revalue(lemMeta$Words, c("ljusare"="bright","lägre"="low","djupa"="deep",
                                         "högre"="high","mörkare"="dark", "tunn"="thin","mjukare"="soft",
                                         "tyngre"="heavy","tunnare"="thin","låga"="low",
                                         "ljus"="bright","ljust"="bright","ljusa"="bright",
                                         "mörk"="dark","mörka"="dark","mörkt"="dark",
                                         "hög"="high","höjd"="high","högt"="high","höga"="high",
                                         "låg"="low","lågt"="low","högre upp"="high up",
                                         "högt upp"="high up","upp"="up","över"="over",
                                         "under"="under","djup"="deep","första"="first",
                                         "andra"="other","ner"="down","kalin"="thick","ince"="thin","yuksek"="high",
                                         "inceydi"="thin","kalindi"="thick","kalinlikta"="thick","asagda"="down",
                                         "asaya"="down","asarda"="down", "yukseklerde"="high","yuksekte"="high","yukseliyor"="high",
                                         "yuksekteydi"="high","dusuk"="low","kalina"="thick","koyu"="dark","altan"="below","acik"="bright",
                                         "alcak"="low","kalinlik"="thick","inceye"="thin","tok"="full","yukariya"="high",
                                         "asardaydi"="down","yukarda"="high","yuksekti" = "high", "kalindi" = "thick","yumsak"="soft",
                                       "ustte"="above","ustunde"="above","kalinca"="thick","dusus"="fall"))
lemMeta=data.table(na.omit(lemMeta))

# Swedish
lemMetaSwe=lemMeta[lemMeta$Language=="Swedish"]
lemMetaSwe[,Freq := .N, by=Lemma]
lemMetaSwe=lemMetaSwe[,-2]
lemMetaSwe=unique(lemMetaSwe)
lemMetaSwe=lemMetaSwe[order(-Freq)]
lemMetaSwe=lemMetaSwe[1:10,]

LemSwe=ggplot(lemMetaSwe,aes(reorder(Lemma,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Metaphor),colour="gray30") +
  #scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  #scale_fill_manual("Lemma",values=cols_4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Swedish lemmata grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemSwe

# Turkish
lemMetaTur=lemMeta[lemMeta$Language=="Turkish"]
lemMetaTur[,Freq := .N, by=Lemma]
lemMetaTur=lemMetaTur[,-2]
lemMetaTur=unique(lemMetaTur)
lemMetaTur=lemMetaTur[order(-Freq)]
lemMetaTur=lemMetaTur[1:10,]

LemTur=ggplot(lemMetaTur,aes(reorder(Lemma,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Metaphor),colour="gray30") +
  #scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  #scale_fill_manual("Lemma",values=cols_4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Turkish lemmata grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemTur


#-------------------------------------------------------------------------------
## ALL FIGURES IN ONE
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(BarSwe,LemSwe,Bartur,LemTur,cols=2)
