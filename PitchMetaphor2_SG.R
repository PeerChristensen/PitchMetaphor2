# PITCH METAPHOR STUDY 2: SPEECH & GESTURE

# DESCRIPTION:

# 1. Metaphors and gesture elicitation
# 2. Metaphors and co-expressivity

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------

### LOAD DATA AND PACKAGES, OMIT VARIABLES
library(ggplot2)
library(dplyr)
library(Hmisc)
library(tidyr)

df = read.csv2("PitchMetaphor2_CLEAN_edit_gesture.csv",header=T,stringsAsFactors=FALSE)
df = df[,-c(1:3)]
df$Gesture[df$Gesture=="n/a"] = "0"
df$Gesture[df$Gesture==""] = "0"
df$Dimension[df$Dimension=="vert" & df$Gesture=="0"] = ""

# Categorise metaphors with < 10 obs. as "Other"
# Or < 3 % : prop.table(ftable(allMetaphors),2)*100
df$Metaphor[df$Metaphor=="?" | df$Metaphor=="Mildness" | df$Metaphor=="Strength" |
              df$Metaphor=="Openness" | df$Metaphor=="Roughness" |
              df$Metaphor=="Roundness" | df$Metaphor=="Sharpness"] = "Other"

#-------------------------------------------------------------------------------
### NUMBER OF OBSERVATIONS WITH GESTURE
numObs = nrow(df)
# By language
ObsByLang = table(df$Language)

#-------------------------------------------------------------------------------
### METAPHORS AND CO-EXPRESSIVITY - Prepare variables

#overview
table(df$Dimension,df$Handshape)

# simplify data by grouping some handshape values
df$Handshape[grepl("grip",df$Handshape)] = "grip"
df$Handshape[df$Handshape == "none" | df$Handshape == "round"| df$Handshape == "open"] = "other"

# Create gesture metaphor variable (height,thickness, mixed, other, none) 
#none
df$GestureExpress[df$Gesture=="0"]= "None"
#other
df$GestureExpress[df$Dimension=="hori" | df$Dimension=="none" & 
                    df$Handshape=="flat V" | df$Dimension=="none" & df$Handshape=="other" | 
                    df$Dimension=="none" & df$Handshape=="point"] = "Other"
# height
df$GestureExpress[df$Dimension=="vert"] = "Vertical"
df$GestureExpress[df$Dimension=="vert" & df$Handshape=="flat H" | df$Handshape=="flat V" |
                    df$Handshape=="point"] = "Vertical"
df$GestureExpress[df$Handshape=="flat H"] = "Vertical"

#thickness
df$GestureExpress[df$Dimension=="none" & df$Handshape=="grip"] = "Thickness"
df$GestureExpress[df$Dimension=="size"] = "Thickness"

#mixed
df$GestureExpress[df$Dimension=="vert" & df$Handshape=="grip"] = "Mixed"

#verify that values are correct
table(df$Dimension,df$Handshape,df$GestureExpress)

#-------------------------------------------------------------------------------
### CREATE FIGURES

#Swedish
dfS=df[df$Language=="Swedish",]

#weights
dfWt = dfS %>% select(Participant, Metaphor,GestureExpress) %>%
  group_by(Participant,Metaphor) %>%
  count()

dfx = dfS %>% select(Participant, Metaphor,GestureExpress) %>%
  group_by(Participant,Metaphor,GestureExpress) %>%
  count() %>%
  spread(key=GestureExpress,value=n)

dfx[is.na(dfx)] = 0
dfx$wt= rowSums(dfx[,3:7])

dfx[,3:7]=round(dfx[,3:7]/dfx$wt,3)
  
expS=ddply(dfx,"Metaphor",summarise,
            V = weighted.mean(Vertical,wt),
            Th = weighted.mean(Thickness,wt),
            M = weighted.mean(Mixed,wt),
            O = weighted.mean(Other,wt),
            N = weighted.mean(None,wt),
            Vse=sqrt(wtd.var(Vertical,wt))/sqrt(16),
            Tse=sqrt(wtd.var(Thickness,wt))/sqrt(16),
            Mse=sqrt(wtd.var(Mixed,wt))/sqrt(16),
            Ose=sqrt(wtd.var(Other,wt))/sqrt(16),
            Nse=sqrt(wtd.var(None,wt))/sqrt(16))

expS[,2:11]=round(expS[,2:11],3)
expS=melt(expS)
expS=data.frame(cbind(expS[1:30,],
                       expS[31:60,2:3]))

barExpS <- ggplot(expS, aes(x=Metaphor, y=value,fill=variable)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_manual("Co-expressivity",values=cols_2) +
  #scale_fill_viridis("Co-expressivity",discrete = TRUE, option = "D") +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("Brightness", "Height","Mass","Other","Softness","Thickness"),
                   labels=c(paste("Brightness\nn =",table(dfS$Metaphor)[1]), 
                            paste("Height\nn =",table(dfS$Metaphor)[2]),
                            paste("Mass\nn =",table(dfS$Metaphor)[3]),
                            paste("Other\nn =",table(dfS$Metaphor)[4]),
                            paste("Softness\nn =",table(dfS$Metaphor)[5]),
                            paste("Thickness\nn =",table(dfS$Metaphor)[6]))) +
  xlab("Metaphors") +
  ggtitle("Speech-gesture co-expressivity in Swedish") +
  theme_minimal() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15))
barExpS

#Turkish
dfT=df[df$Language=="Turkish",]
dfT$Metaphor[dfT$Metaphor=="Brightness"|dfT$Metaphor=="Softness"]="Other"

dfx = dfT %>% select(Participant, Metaphor,GestureExpress) %>%
  group_by(Participant,Metaphor,GestureExpress) %>%
  count() %>%
  spread(key=GestureExpress,value=n)

dfx[is.na(dfx)] = 0
dfx$wt= rowSums(dfx[,3:7])

dfx[,3:7]=round(dfx[,3:7]/dfx$wt,3)

expT=ddply(dfx,"Metaphor",summarise,
           V = weighted.mean(Vertical,wt),
           Th = weighted.mean(Thickness,wt),
           M = weighted.mean(Mixed,wt),
           O = weighted.mean(Other,wt),
           N = weighted.mean(None,wt),
           Vse=sqrt(wtd.var(Vertical,wt))/sqrt(16),
           Tse=sqrt(wtd.var(Thickness,wt))/sqrt(16),
           Mse=sqrt(wtd.var(Mixed,wt))/sqrt(16),
           Ose=sqrt(wtd.var(Other,wt))/sqrt(16),
           Nse=sqrt(wtd.var(None,wt))/sqrt(16))

expT[,2:11]=round(expT[,2:11],3)
expT=melt(expT)
expT=data.frame(cbind(expT[1:15,],
                      expT[16:30,2:3]))

barExpT <- ggplot(expT, aes(x=Metaphor, y=value,fill=variable)) + 
  geom_bar(position=position_dodge(),stat="identity",colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  #scale_fill_manual("Co-expressivity",values=cols_2) +
  #scale_fill_viridis("Co-expressivity",discrete = TRUE, option = "D") +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("Height", "Other","Thickness"),
                   labels=c(paste("Height\nn =",table(dfT$Metaphor)[1]),
                             paste("Other\nn =",table(dfT$Metaphor)[2]),
                             paste("Thickness\nn =",table(dfT$Metaphor)[3]))) +
  xlab("Metaphors") +
  ggtitle("Speech-gesture co-expressivity in Turkish") +
  theme_minimal() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15))
barExpT
