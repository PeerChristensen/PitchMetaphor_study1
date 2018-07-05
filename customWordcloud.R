# custom shape wordclouds

library(wordcloud2)
library(viridis)
library(plyr)
library(data.table)

df_full= read.csv2("all_data_CLEAN.csv",na.strings = c(""),stringsAsFactors = F)

df=df_full[,-c(1,3,13,14,15,16,17,18)]
df=df[!duplicated(df), ]

wordsMeta=df[,c(5,6,11)]
wordsMeta=wordsMeta[wordsMeta$Scope!="none" & wordsMeta$Scope!="NA",]
wordsMeta$Scope[wordsMeta$Words=="tok"]="Fullness"
wordsMeta=data.table(na.omit(wordsMeta))

wordsMeta[,Freq := .N, by=Words]
wordsMeta=unique(wordsMeta)
wordsMeta=wordsMeta[order(-Freq)]
wordsMeta=data.frame(wordsMeta[,c(2,4)])
wordsMeta=wordsMeta[1:44,]

cols=viridis(100, alpha = 1, begin = 0, end = 1, direction = 1)

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

wordsLem[,Freq := .N, by=Words]
wordsLem=unique(wordsLem)
wordsLem=wordsLem[order(-Freq)]
wordsLem=data.frame(wordsLem[,c(3,4)])

wordcloud(words = wordsLem$Lem, freq = wordsLem$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=viridis_pal(option="B", begin = .1, end =.8)(10))
            #brewer.pal(8, "Dark2")
ggsave("cloud.png")

wordcloud2(wordsLem,figPath = "gesture1.png",size=3,
           color=viridis_pal(option="B", begin = .1, end =.8)(10))

wordcloud2(wordsLem,figPath = "gkey.png",
           color=viridis_pal(option="B", begin = .1, end =.8)(10),
            size=5,maxRotation = pi/3)


