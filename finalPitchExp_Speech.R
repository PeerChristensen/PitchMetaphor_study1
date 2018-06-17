# FINAL ANAlYSIS MUSIC EXP DATA: SPEECH
# PEER CHRISTENSEN
# OCTOBER 2017

library(data.table)
library(ggplot2)
library(viridis)
library(Hmisc)
library(plyr)

df_full= read.csv2("all_data_CLEAN.csv",na.strings = c(""),stringsAsFactors = F)
#remove some columns
df=df_full[,-c(1,3,13,14,15,16,17,18)]
df=df[!duplicated(df), ]

#select colors for plots
cols_1=viridis(10)[7]
cols_2=viridis(6)[c(2,6)]
cols_4=viridis(10)[c(2,5,8,10)]
cols_5=viridis(10)[c(2,4,7,9,10)]

########## 1. WORDS ########

#### 1.1 N observations ###
# Total observations with metaphorical expressions
nrow(df)
# By language
table(df$Language)

#### 1.2 Words by metaphor ###
wordsMeta=df[,c(5,6,11)]
wordsMeta=wordsMeta[wordsMeta$Scope!="none" & wordsMeta$Scope!="NA",]
wordsMeta$Scope[wordsMeta$Words=="tok"]="Fullness"
wordsMeta=data.table(na.omit(wordsMeta))

#Swedish
wordsMetaSwe=wordsMeta[wordsMeta$Language=="Swedish"]
wordsMetaSwe[,Freq := .N, by=Words]
wordsMetaSwe=unique(wordsMetaSwe)
wordsMetaSwe=wordsMetaSwe[order(-Freq)]
wordsMetaSwe=wordsMetaSwe[1:15,]

#wordsMetaSwe=data.frame(sort(table(wordsMetaSwe$Scope), decreasing=T))

wmSwe=ggplot(wordsMetaSwe,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Scope),colour="gray30") +
  #scale_fill_viridis("Metaphor",discrete = TRUE, option = "D") +
  scale_fill_manual("Metaphor",values=cols_2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
wmSwe

#Turkish
wordsMetaTur=wordsMeta[wordsMeta$Language=="Turkish"]
wordsMetaTur$Scope[wordsMetaTur=="tok"]="Fullness"
wordsMetaTur[,Freq := .N, by=Words]
wordsMetaTur=unique(wordsMetaTur)
wordsMetaTur=wordsMetaTur[order(-Freq)]
wordsMetaTur=wordsMetaTur[1:15,]
wmTur=ggplot(wordsMetaTur,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Scope),colour="gray30") +
  #scale_fill_viridis("Metaphor",discrete = TRUE, option = "D") +
  scale_fill_manual("Metaphor",values=cols_4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Turkish grouped by metaphor") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
wmTur

#### 1.3 Words by PoS  ###
wordsPoS=df[,c(6,11)]
wordsPoS$PoS=revalue(wordsPoS$Words, c("ljusare"="Adj-C","lägre"="Adj-C",
                           "högre"="Adj-C","mörkare"="Adj-C",
                           "ljus"="Adj-P","ljust"="Adj-P","ljusa"="Adj-P",
                           "mörk"="Adj-P","mörka"="Adj-P","mörkt"="Adj-P",
                           "hög"="Adj-P","högt"="Adj-P","höga"="Adj-P",
                           "låg"="Adj-P","lågt"="Adj-P","högre upp"="ADV",
                           "högt upp"="ADV","upp"="ADV","över"="PREP",
                           "under"="PREP","djup"="Adj-P","första"="REF",
                           "andra"="REF","ner"="ADV","kalin"="Adj","ince"="Adj","yuksek"="Adj",
                           "inceydi"="Adj-Past","kalindi"="Adj-Past","kalinlikta"="N",
                           "dusuk"="Adj","kalina"="Adj-Dat","koyu"="Adj","altan"="Adj-Abl",
                           "alcak"="Adj","kalinlik"="N","inceye"="Adj-Dat","tok"="Adj","yukariya"="Adj-Dat"))

wordsPoS=data.table(na.omit(wordsPoS))

#Swedish
wordsPoSSwe=wordsPoS[wordsPoS$Language=="Swedish"]
wordsPoSSwe[,Freq := .N, by=Words]
wordsPoSSwe=unique(wordsPoSSwe)
wordsPoSSwe=wordsPoSSwe[order(-Freq)]
wordsPoSSwe=wordsPoSSwe[1:15,]

PoSSwe=ggplot(wordsPoSSwe,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=PoS),colour="gray30") +
  #scale_fill_viridis("PoS",discrete = TRUE, option = "D") +
  scale_fill_manual("PoS",values=cols_2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by Parts of speech") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
PoSSwe

#Turkish
wordsPoSTur=wordsPoS[wordsPoS$Language=="Turkish"]
wordsPoSTur[,Freq := .N, by=Words]
wordsPoSTur=unique(wordsPoSTur)
wordsPoSTur=wordsPoSTur[order(-Freq)]
wordsPoSTur=wordsPoSTur[1:15,]

PoSTur=ggplot(wordsPoSTur,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=PoS),colour="gray30") +
  #scale_fill_viridis("PoS",discrete = TRUE, option = "D") +
  scale_fill_manual("PoS",values=cols_5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Turkish grouped by Parts of speech") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
PoSTur

### 1.4a Words by lemmata ###
wordsLem=df[,c(6,11)]
wordsLem$Lem=revalue(wordsPoS$Words, c("ljusare"="bright","lägre"="low","djupa"="deep",
                                       "högre"="high","mörkare"="dark",
                                       "ljus"="bright","ljust"="bright","ljusa"="bright",
                                       "mörk"="dark","mörka"="dark","mörkt"="dark",
                                       "hög"="high","höjd"="high","högt"="high","höga"="high",
                                       "låg"="low","lågt"="low","högre upp"="high up",
                                       "högt upp"="high up","upp"="up","över"="over",
                                       "under"="under","djup"="deep","första"="first",
                                       "andra"="other","ner"="down","kalin"="thick","ince"="thin","yuksek"="high",
                                       "inceydi"="thin","kalindi"="thick","kalinlikta"="thick",
                                       "dusuk"="low","kalina"="thick","koyu"="dark","altan"="below",
                                       "alcak"="low","kalinlik"="thick","inceye"="thin","tok"="full","yukariya"="high"))

wordsLem=data.table(na.omit(wordsLem))

#Swedish
wordsLemSwe=wordsLem[wordsLem$Language=="Swedish"]
wordsLemSwe[,Freq := .N, by=Words]
wordsLemSwe=unique(wordsLemSwe)
wordsLemSwe=wordsLemSwe[order(-Freq)]
wordsLemSwe=wordsLemSwe[1:15,]

LemSwe=ggplot(wordsLemSwe,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Lem),colour="gray30") +
  #scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  scale_fill_manual("Lemma",values=cols_4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Swedish grouped by lemmata") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemSwe

#Turkish
wordsLemTur=wordsLem[wordsLem$Language=="Turkish"]
wordsLemTur[,Freq := .N, by=Words]
wordsLemTur=unique(wordsLemTur)
wordsLemTur=wordsLemTur[order(-Freq)]
wordsLemTur=wordsLemTur[1:15,]

LemTur=ggplot(wordsLemTur,aes(reorder(Words,Freq),Freq)) + 
  geom_bar(stat = "identity",aes(fill=Lem),colour="gray30") +
  scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Words in Turkish grouped by lemmata") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemTur

### 1.4b Lemmata  ###
wordsLem2=wordsLem[,-1]

#Swedish
wordsLemSwe2=wordsLem2[wordsLem2$Language=="Swedish"]
wordsLemSwe2[,Freq := .N, by=Lem]
wordsLemSwe2=unique(wordsLemSwe2)
wordsLemSwe2=wordsLemSwe2[order(-Freq)]
wordsLemSwe2=wordsLemSwe2[1:5,]

LemSwe2=ggplot(wordsLemSwe2,aes(reorder(Lem,Freq),Freq)) + 
  geom_bar(stat = "identity",fill=cols_1,colour="gray30") +
  #scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("The most frequently used lemmata in Swedish") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemSwe2

#Turkish
wordsLemTur2=wordsLem2[wordsLem2$Language=="Turkish"]
wordsLemTur2[,Freq := .N, by=Lem]
wordsLemTur2=unique(wordsLemTur2)
wordsLemTur2=wordsLemTur2[order(-Freq)]
wordsLemTur2=wordsLemTur2[1:5,]

LemTur2=ggplot(wordsLemTur2,aes(reorder(Lem,Freq),Freq)) + 
  geom_bar(stat = "identity",fill=cols_1,colour="gray30") +
  #scale_fill_viridis("Lemma",discrete = TRUE, option = "D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("The most frequently used lemmata in Turkish") +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
LemTur2

### 1.5 Subject ####
dfSubj=df[df$Subject!="clip" & df$Subject!="instrument"&
          df$Subject!="NA" & df$Subject!="part",]
dfSubj$Subject[dfSubj$Subject=="voice"|dfSubj$Subject=="gender"]="gender+voice"
Subjects=round(prop.table(table(dfSubj$Subject,dfSubj$Language),2),2)
barplot(Subjects,beside=T,legend=T,ylim=c(0,1))

########## 2. METAPHORS ########

#### 2.1 Metaphor usage  ####

#Swedish
dfS=df[df$Language=="Swedish",]
dfS=ftable(dfS$File,dfS$Scope)
wtS=rowSums(dfS)
dfS=round(prop.table(as.matrix(dfS),1),2)
dfS=data.frame(dfS,wtS)
dfS$Language="Swedish"

dfS=ddply(dfS,"Language",summarise,
                   H= weighted.mean(Height,wtS), 
                   B=weighted.mean(Brightness,wtS),
                   Hse=sqrt(wtd.var(Height,wtS))/sqrt(nrow(dfS)),
                   Bse=sqrt(wtd.var(Brightness,wtS))/sqrt(nrow(dfS)))

dfS <- transform(dfS, Hlower=H-Hse, Hupper=H+Hse,Blower=B-Bse,
                          Bupper=B+Bse)

dfS=melt(dfS,Language=Language)

dfS=data.frame(cbind(dfS[1:2,],
                              dfS[3:4,2:3]))
dfS[,c(3,5)]=round(dfS[,c(3,5)],3)

BarSwe <- ggplot(dfS, aes(x=variable, y=value, fill=Language)) + 
  geom_bar(position=position_dodge(),stat="identity",fill="#21908CFF" ,colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = value, y = 0.3, size = 3)) +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("H","B"),
                   labels=c("Height", "Brightness")) +
  xlab("Metaphors") +
  ggtitle("Swedish pitch metaphors in speech") +
  theme_minimal() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
BarSwe

#Turkish
dfT=df[df$Language=="Turkish",]
#we collapse "Brightness" and "Other"
dfT$Scope[dfT$Scope=="Brightness"]="Other"
dfT=ftable(dfT$File,dfT$Scope)
wtT=rowSums(dfT)
dfT=round(prop.table(as.matrix(dfT),1),2)
dfT=data.frame(dfT,wtT)
dfT$Language="Turkish"

dfT=ddply(dfT,"Language",summarise,
          H= weighted.mean(Height,wtT), 
          Th=weighted.mean(Thickness,wtT),
          O=weighted.mean(Other,wtT),
          Hse=sqrt(wtd.var(Height,wtT))/sqrt(nrow(dfT)),
          Thse=sqrt(wtd.var(Thickness,wtT))/sqrt(nrow(dfT)),
          Ose=sqrt(wtd.var(Other,wtT))/sqrt(nrow(dfT)))

dfT <- transform(dfT, Hlower=H-Hse, Hupper=H+Hse,Thlower=Th-Thse,
                 Thupper=Th+Thse,Olower=O-Ose,Oupper=O+Ose)

dfT=melt(dfT,Language=Language)

dfT=data.frame(cbind(dfT[1:3,],
                     dfT[4:6,2:3]))

dfT[,c(3,5)]=round(dfT[,c(3,5)],3)

BarTur <- ggplot(dfT, aes(x=variable, y=value, fill=Language)) + 
  geom_bar(position=position_dodge(),stat="identity",fill="#21908CFF" ,colour="gray30") +
  geom_errorbar(aes(ymin=value-value.1, ymax=value+value.1), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = value, y = 0.3, size = 3)) +
  scale_y_continuous(name="Mean Proportions", limits=c(0, 1)) +
  scale_x_discrete(breaks=c("H","Th","O"),
                   labels=c("Height","Thickness","Other")) +
  xlab("Metaphors") +
  ggtitle("Turkish pitch metaphors in speech") +
  theme_minimal() +
  theme(plot.title=element_text(lineheight=0.8,hjust=0.5,face="bold", size=20),
        axis.title.x=element_text(face="bold",size=17), 
        axis.text.x=element_text(size=15),
        axis.title.y=element_text(face="bold",size=17),
        axis.text.y=element_text(size=15),
        legend.position = "none")
BarTur

#### 2.2 Consistency over time ####
#Swedish height and Turkish thickness, lineplot with error bars

#Swedish
conS=df[df$Language=="Swedish" & !df$Trial>25,]
conS=conS[,-c(4,6,7,8,9,10,11)]
conS=conS[,c(1,2,4)]
wMeans=c()
wSEs=c()
for(i in seq(1,25)){
  t=conS[conS$Trial==i,]
  fTab=ftable(t,col.vars = "File")
  cSum=colSums(fTab)
  fTab=round(prop.table(as.matrix(fTab),2),2)
  fTab=rbind(fTab,cSum)
  wMean=weighted.mean(fTab[2,],fTab[3,])
  wMeans=append(wMeans,wMean)
  wSE=sqrt(wtd.var(fTab[2,],fTab[3,]))/sqrt(ncol(fTab))
  wSEs=append(wSEs,wSE)
}
conS=data.frame(Trial=seq(1,25),wMeans,wSEs)

consistencyS=ggplot(conS,aes(x=Trial, y=wMeans)) +
  geom_line(size = 1.5,color="#21908CFF") +
  geom_point(size=1.5,color="gray30") +
  geom_errorbar(aes(ymin=wMeans-wSEs,ymax=wMeans+wSEs),width=0.3,color="gray30") +
  scale_y_continuous(name="Mean proportion of Height expressions", limits=c(0,1)) +
  ggtitle("Swedish participants' Usage of HEIGHT by trial") +
  ylab("Trial") +
  theme_minimal()
consistencyS

#Turkish
conT=df[df$Language=="Turkish" & !df$Trial>25,]
conT=conT[,-c(4,6,7,8,9,10,11)]
conT=conT[,c(1,2,4)]
conT = conT[order(conT$Trial,conT$File),] 
wMeans=c()
wSEs=c()
for(i in seq(1,25)){
  t=conT[conT$Trial==i,]
  fTab=ftable(t,col.vars = "File")
  cSum=colSums(fTab)
  fTab=round(prop.table(as.matrix(fTab),2),2)
  fTab=rbind(fTab,cSum)
  wMean=weighted.mean(tail(fTab,2)[1,],tail(fTab,2)[2,])
  wMeans=append(wMeans,wMean)
  wSE=sqrt(wtd.var(tail(fTab,2)[1,],tail(fTab,2)[2,]))/sqrt(ncol(fTab))
  wSEs=append(wSEs,wSE)
}
conT=data.frame(Trial=seq(1,25),wMeans,wSEs)

consistencyT=ggplot(conT,aes(x=Trial, y=wMeans)) +
  geom_line(size = 1.5,color="#21908CFF") +
  geom_point(size=1.5,color="gray30") +
  geom_errorbar(aes(ymin=wMeans-wSEs,ymax=wMeans+wSEs),width=0.3,color="gray30") +
  scale_y_continuous(name="Mean proportion of Height expressions", limits=c(0,1.01)) +
  ggtitle("Turkish participants' Usage of THICKNESS by trial") +
  ylab("Trial") +
  theme_minimal()
consistencyT

#simpson's diversity index
# (n*(n-1)) / N(N-1)

#Swedish
conS=df[df$Language=="Swedish" & !df$Trial>25,]
conS=conS[,-c(4,6,7,8,9,10,11)]
conS=conS[,c(1,2,4)]
sDivs=c()
for(i in seq(1,25)){
  t=conS[conS$Trial==i,]
  fTab=ftable(t,col.vars = "File")
  N=sum(fTab)
  n=sum(fTab[2,])
  sDiv= (n*(n-1)) / (N*(N-1))
  sDivs=append(sDivs,sDiv)
}
conS=data.frame(Trial=seq(1,25),sDivs)















###2.3. consistency by participant ###
conS=df[df$Language=="Swedish" & !df$Trial>25,]
conS=conS[,-c(4,6,7,8,9,10,11)]
conS=conS[,c(1,2,4)]
conS$File=as.numeric(gsub("swe", "",conS$File))
wMeans=c()
wSEs=c()
for(i in seq(1,25)){
  t=conS[conS$File==i,]
  fTab=ftable(t,col.vars = "Trial")
  cSum=colSums(fTab)
  fTab=round(prop.table(as.matrix(fTab),2),2)
  fTab=rbind(fTab,cSum)
  wMean=weighted.mean(tail(fTab,2)[1,],tail(fTab,2)[2,])
  wMeans=append(wMeans,wMean)
  wSE=sqrt(wtd.var(tail(fTab,2)[1,],tail(fTab,2)[2,]))/sqrt(ncol(fTab))
  wSEs=append(wSEs,wSE)
}
conS=data.frame(Participant=seq(1,25),wMeans,wSEs)





#t1=conS[conS$Trial==6,]
#t1=ftable(t1$File,t1$Scope)
#wtS=rowSums(t1)
#t1=round(prop.table(as.matrix(t1),1),2)
#t1=data.frame(t1,wtS)
#t1$Language="Swedish"
#conS=prop.table(table(conS),1)

#conS1=data.table(conS)
#conS1[,Freq := .N, by=c(Trial,Scope)]

#conS=ftable(conS$File,conS$Scope)
#wtS=rowSums(conS)
#conS=round(prop.table(as.matrix(conS),1),2)
#conS=data.frame(conS,wtS)
#conS$Language="Swedish"


#solution? with fixed Trial

wtS=as.vector(table(conS$Trial))


a=ftable(t1,col.vars = "File")
b=colSums(a)
a=round(prop.table(as.matrix(a),2),2)
a=rbind(a,b)
wMean=weighted.mean(a[2,],a[3,])



conS$wMean=apply(conS,1,function(x) weighted.mean(x[2],x[3]))

conS$test=conS[,1]*conS[,2]
conS=ddply(conS,"Language",summarise,
           M= weighted.mean(Height,wtS),
           Hse=sqrt(wtd.var(Height,wtS))/sqrt(nrow(conS)))