# PITCH METAPHOR STUDY 2: REGRESSION MODELS

# DESCRIPTION:

# 1. Speech metaphors and language
# 1. Metaphors and gesture elicitation
# 2. Metaphors and co-expressivity

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------
# LOAD DATA

library(lme4)
library(lmerTest)

df = read.csv2("PitchMetaphor2_CLEAN_edit_gesture_2.csv",header=T,stringsAsFactors=FALSE)
df = df[,-1]
cols2factor=c(1,3,4,8,9,10,11,12,17)
df[cols2factor] <- lapply(df[cols2factor], factor)
df$Metaphor[df$Metaphor=="Mass"|df$Metaphor=="Softness"] = "Other"
df=droplevels(df)
df$Trial[df$Trial==16]=15

#ADD DATA FROM LHQ
lhq = read.csv2("LHQ_biling.csv",header=T,stringsAsFactors=FALSE,skip=1)
df=merge(x=df,y=lhq[,1:3],by.x = "Participant",by.y = "Participant.ID")
df$Sex=factor(df$Sex)

#CO-EXPRESSIVITY VARIABLE
df$CoExpress=ifelse(df$Metaphor=="Height" & df$GestureExpress=="Vertical" |
                      df$Metaphor=="Height" & df$GestureExpress=="Mixed" |
                      df$Metaphor=="Thickness" & df$GestureExpress=="Thickness" |
                      df$Metaphor=="Thickness" & df$GestureExpress=="Mixed",1,0)

#VARIABLE FOR LANGUAGE-SPECIFIC METAPHOR
df$LangSpec=ifelse(df$Metaphor =="Height" & df$Language == "Swedish" |
                     df$Metaphor == "Thickness" & df$Language == "Turkish",1,0)

#VARIABLE FOR LANGUAGE SPECIFIC METAPHORS PRODUCED IN "RIGHT"/"WRONG" CONDITION
df$SpillOver=ifelse(df$Metaphor =="Height" & df$Language == "Turkish" |
                      df$Metaphor == "Thickness" & df$Language == "Swedish" |
                      df$Metaphor == "Brightness" & df$Language=="Turkish",1,0)

#VARIABLE FOR VERTICAL GESTURE
df$GestureVert=ifelse(df$GestureExpress=="Vertical",1,0)

#VARIABLE FOR SPATIAL METAPHOR
df$Space=ifelse(df$Metaphor=="Height" | df$Metaphor=="Thickness",1,0)
# ----------------------------------------------------------------------------------------------------
### SPEECH

# 1. likelihood of using height in Swedish vs. thickness in Turkish
prop.table(table(df$Language,df$LangSpec),1)

#without Language
mod1a = glmer(LangSpec ~ Order + Trial + Sex + Age + (1+Language|Participant),data=df, family="binomial")
summary(mod1a)
#With Language
mod1b = glmer(LangSpec ~ Language + Order + Trial + Sex + Age + (1+Language|Participant),data=df, family="binomial")
summary(mod1b)

anova(mod1a,mod1b)

# 2. Are "spill over" metaphors predicted by Order and language?
mod2a = glmer(SpillOver ~Trial + Language + Sex + Age +(1+Language|Participant),data=df, family="binomial")
summary(mod2a)
mod2b = glmer(SpillOver ~ Order + Trial + Language + Sex + Age +(1+Language|Participant),data=df, family="binomial")
summary(mod2b)

anova(mod2a,mod2b)

# ----------------------------------------------------------------------------------------------------
### GESTURE

# GESTURE ELICITATION ~ METAPHOR + LANGUAGE + ORDER + TRIAL
df$Metaphor=relevel(df$Metaphor,ref="Other")
mod3 = glmer(Gesture ~ Metaphor + Language + Order + Trial + Age + Sex + (1+Language|Participant),data=df, family="binomial")
summary(mod3)

# VERTICAL GESTURE ELICITATION
df$Metaphor=relevel(df$Metaphor,ref="Height")
mod4 = glmer(GestureVert ~ Metaphor + Language + Order + Trial + Age + Sex + (1+Language|Participant),data=df, family="binomial")
summary(mod4)

# SPEECH-GESTURE CO-EXPRESSIVITY ~ METAPHOR + LANGUAGE + ORDER + TRIAL
df2=subset(df,Gesture==1 & Metaphor=="Height" | Metaphor=="Thickness")
df2$Metaphor=relevel(df2$Metaphor,ref="Thickness")
mod5 = glmer(CoExpress~ Metaphor + Language + Order + Trial + Age + Sex + (1+Language|Participant),data=df2, family="binomial")
summary(mod5)

# GESTURE MAPPING ~ METAPHOR + LANGUAGE + ORDER + TRIAL
df3=df2
df3=df3[df3$GestureExpress=="Vertical"|df3$GestureExpress=="Thickness",]
df3=droplevels(df3)
mod6 = glmer(GestureExpress~ Metaphor + Language + Order + Trial + Age + Sex + (1+Language|Participant),data=df3, family="binomial")
summary(mod6)


#code for changing coefficients (logits) to probabilities
#exp(summary(mod5)$coefficients[,1])
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
#logit2prob(coef(summary(mod5))[,1])
