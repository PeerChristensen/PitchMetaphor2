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
write.csv2(df,"PitchMetaphor2_CLEAN.csv")

### WRITE AGAIN (AFTER MANUAL EDITS) - NEED TO CHANGE CHARACTERS AGAIN
df = read.csv2("PitchMetaphor2_CLEAN_edit.csv")

df$Words=gsub("√∂","ö",df$Words)
df$Words=gsub("√•","å",df$Words)
df$Words=gsub("√§","ä",df$Words)

write.csv2(df,"PitchMetaphor2_CLEAN_edit.csv")


### CREATE FILE WITH GENERAL COMMENTS AND FILE NAME
df2 = read.csv2("data_original.csv")

comments = df2[,c(1,16)]
comments = comments[comments$General.comments!="",]
comments = comments[!duplicated(comments),]

write.csv2(comments,"comments.csv")



