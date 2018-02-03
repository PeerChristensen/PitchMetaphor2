# PITCH METAPHOR STUDY 2: SPEECH

# DESCRIPTION:


#AUTHOR: PEER CHRISTENSEN
#FEBRUARY 2018
#-------------------------------------------------------------------------------

### LOAD DATA AND PACKAGES, OMIT VARIABLES
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


### WORDS

# By metaphor

# By lemma

#-------------------------------------------------------------------------------

### LEMMATA