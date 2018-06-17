# CLEANING MUSIC EXP DATA
#October 2017

library(plyr)

#READ AND CLEAN DATA
setwd("/Users/peerchristensen/Desktop")
df= read.csv2("all_data.csv",na.strings = c(""))
df=df[!is.na(df$Trial) & !is.na(df$Part) & 
        !is.na(df$Words) &!is.na(df$Gesture),]
df$Language = as.factor(ifelse(grepl("t",df$File), "Turkish","Swedish"))
df=df[,-18]
df=df[!duplicated(df), ]

df$Words=gsub("m̦","mö",df$Words)
df$Words=gsub("b̦","bö",df$Words)
df$Words=gsub("h̦","hö",df$Words)
df$Words=gsub("l̴","lå",df$Words)
df$Words=gsub("\xcc_","ä",df$Words)
df$Scope[df$Scope=="Height"&df$Words=="mörkare"]="Brightness"
df$Scope[df$Scope=="Thickness"&df$Words=="ljust"]="Brightness"
df$Scope[df$Words=="kalin" | df$Words=="kalinligi" | df$Words=="ince"]="Thickness"
df$Scope[df$Words=="yukariya"]="Height"
df$Words[df$Words=="thin"]="ince"
df$Scope[df$Words=="dov"] = "none"
df$Scope[grepl("mörk",df$Words)==T | grepl("ljus",df$Words)==T]="Brightness"
df$Scope[grepl("hög",df$Words)==T]="Height"
df$Scope[is.na(df$Scope)]="none"
df=subset(df,df$Scope!="none")

#WRITE NEW CSV FILES
write.csv2(df,"all_data_CLEAN.csv")
dfG=df[df$Gesture==1,]
write.csv2(dfG,"all_gesture_data_CLEAN.csv")
