# Peer Christensen
# Script for randomly selecting gesture data for independent coding

# install/load xtable
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xtable)

# load and subset data
df= read.csv2("PitchMetaphor2_CLEAN_edit_gesture_2.csv",na.strings = c(""))
df=df[df$Gesture==1,]

# number of observations for independent coding
totalRows=nrow(df)
subsetRows=round(totalRows/100*15,0) # 15 % of the data

# select observations
set.seed(2452)
rows2code=sample(totalRows,subsetRows,replace=F)
dfCode=df[rows2code,c(2,4,3,6,7,14)] # get rows and cols, i.e. participant, language, trial, part, time
dfCode=dfCode[with(dfCode,order(Participant,Language,Time,Trial,Part)),] #sort data frame

# export data frame as html table
codeTab<-xtable(dfCode)
print.xtable(codeTab, type="html", file="PitchMetaphor2_dataset_IRR_update.html")
