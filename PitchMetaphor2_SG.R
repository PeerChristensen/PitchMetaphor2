# PITCH METAPHOR STUDY 2: SPEECH & GESTURE

# DESCRIPTION:

# 1. Metaphors and gesture elicitation
# 2. Metaphors and co-expressivity

#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------

### LOAD DATA AND PACKAGES, OMIT VARIABLES
library(ggplot2)
library(plyr)
library(Hmisc)
library(reshape)
library(data.table)

df = read.csv2("PitchMetaphor2_CLEAN_edit_gesture.csv",header=T,stringsAsFactors=FALSE)
df = df[,-c(1:3)]
dfG=df[df$Gesture==1,]
# Categorise metaphors with < 10 obs. as "Other"
# Or < 3 % : prop.table(ftable(allMetaphors),2)*100
dfG$Metaphor[dfG$Metaphor=="?" | dfG$Metaphor=="Mildness" | dfG$Metaphor=="Strength" |
              dfG$Metaphor=="Openness" | dfG$Metaphor=="Roughness" |
              dfG$Metaphor=="Roundness" | dfG$Metaphor=="Sharpness"] = "Other"

#-------------------------------------------------------------------------------
### NUMBER OF OBSERVATIONS WITH GESTURE
numObs = nrow(dfG)
# By language
ObsByLang = table(dfG$Language)

#-------------------------------------------------------------------------------
### METAPHORS AND GESTURE ELICITATION

#-------------------------------------------------------------------------------
### METAPHORS AND CO-EXPRESSIVITY

# Create gesture metaphor variable (height,thickness, mixed, none) 




