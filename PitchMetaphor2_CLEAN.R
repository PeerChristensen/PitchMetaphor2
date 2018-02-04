# PITCH METAPHOR STUDY 2: DATA CLEANING

### LOAD DATA
df = read.csv2("data_original.csv")

### REMOVE EMPTY ROWS
df = df[df$Words!="" & df$Part!="",]
df = df[is.na(df$File)!=T,]
df = df[df$Metaphor!="" | df$Metaphor!="none",]
df=droplevels(df)

### FILL LANGUAGE VARIABLE
df$Language = "Swedish"
df$Language[grepl("t",df$File)] = "Turkish"

### FILL ORDER VARIABLE
df$Order = "S-T"
df$Order[grepl("1t",df$File) | grepl("2s",df$File) ] = "T-S"

### CHANGE FILE VARIABLE TO PARTICIPANT NUMBER
df$File=gsub(".*bi|_.*", "", df$File)
colnames(df)[1] = "Participant"

### CHANGE CHARACTERS
df$Words=gsub("√∂","ö",df$Words)
df$Words=gsub("√•","å",df$Words)
df$Words=gsub("√§","ä",df$Words)

### WRITE NEW DATAFILE
#write.csv2(df,"PitchMetaphor2_CLEAN.csv")

### WRITE AGAIN (AFTER MANUAL EDITS) - NEED TO CHANGE Metaphor labels
df = read.csv2("PitchMetaphor2_CLEAN_edit.csv")

df2=df
df2$Metaphor[df2$Words=="ince"] = "Thickness"
df2$Metaphor[df2$Words=="kalin"] = "Thickness"
df2$Metaphor[df2$Words=="koyu"] = "Brightness"

#write.csv2(df2,"PitchMetaphor2_CLEAN_edit_2.csv")

df3=df2
df3$Metaphor[df3$Words=="högt"] = "Height"

#write.csv2(df3,"PitchMetaphor2_CLEAN_edit_3.csv")

### INCLUDE GESTURES (where removed by mistake)
df4=df
df4$Metaphor[df4$Words=="ince"] = "Thickness"
df4$Metaphor[df4$Words=="kalin"] = "Thickness"
df4$Metaphor[df4$Words=="koyu"] = "Brightness"
df4$Metaphor[df4$Words=="högt"] = "Height"
#write.csv2(df4,"PitchMetaphor2_CLEAN_edit_gesture.csv")


### CREATE FILE WITH GENERAL COMMENTS AND FILE NAME
df2 = read.csv2("data_original.csv")

comments = df2[,c(1,16)]
comments = comments[comments$General.comments!="",]
comments = comments[!duplicated(comments),]

#write.csv2(comments,"comments.csv")



