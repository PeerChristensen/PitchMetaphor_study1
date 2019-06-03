# FINAL ANAlYSIS MUSIC EXP DATA: INTERRATER RELIABILITY
# PEER CHRISTENSEN
# OCTOBER 2017

library(irr)
library(plyr)
library(psych)
library(xtable)

df= read_csv2("all_data_IRR.csv")
df=df[,-c(2,14)]
df=df[!is.na(df$DimensionIL),]
df=df[!duplicated(df), ]
df$Language=ifelse(grepl("swe",df$File)==T,"Swedish","Turkish")
df$DimensionIL=revalue(df$DimensionIL,c(other="none"))
df$Handshape=revalue(df$Handshape, c("narow grip"="grip","narrow grip"="grip",
                                     "neutral grip"="grip","wide grip"="grip",
                                     "none"="other","open"="other"))
df$HandshapeIL=revalue(df$HandshapeIL, c("narow grip"="grip","narrow grip"="grip",
                                     "neutral grip"="grip","wide grip"="grip",
                                     "none"="other","open"="other"))

df$DimensionIL[grepl("hori",df$CommentsIL)==T]="hori"
df=droplevels(df)
dfS=df[df$Language=="Swedish",]
dfT=df[df$Language=="Turkish",]

#DIMENSION - Total
kappa2(cbind(df$Dimension,df$DimensionIL))
cohen.kappa(cbind(df$Dimension, df$DimensionIL))
prop.table(table(df$Dimension==df$DimensionIL))

#HANDSHAPE - Total
kappa2(cbind(df$Handshape,df$HandshapeIL))
cohen.kappa(cbind(df$Handshape, df$HandshapeIL))
prop.table(table(df$Handshape==df$HandshapeIL))

#DIMENSION - Swedish
kappa2(dfS[,c(7,8)])

#HANDSHAPE - Swedish
kappa2(dfS[,c(9,10)])

#DIMENSION - Turkish
kappa2(dfT[,c(7,8)])

#HANDSHAPE - Turkish
kappa2(dfT[,c(9,10)])


#Table using kappa2from "irr" package
CohenKappa=data.frame(Group=c("Total","Swedish","Turkish"),
           Dimension=round(c(kappa2(df[,c(7,8)])$value,kappa2(dfS[,c(7,8)])$value,kappa2(dfT[,c(7,8)])$value),3),
           Handshape=round(c(kappa2(df[,c(9,10)])$value,kappa2(dfS[,c(9,10)])$value,kappa2(dfT[,c(9,10)])$value),3))
CohenKappa

#Table using cohen.kappa from "psych" package with confidence intervals
KappaConfint=t(data.frame("Dimension total"=cohen.kappa(df[,c(7,8)])$confid[1,],
                        "Handshape total"=cohen.kappa(df[,c(9,10)])$confid[1,],
                        "Dimension Swedish"=cohen.kappa(dfS[,c(7,8)])$confid[1,],
                        "Dimension Turkish"=cohen.kappa(dfT[,c(7,8)])$confid[1,],
                        "Handshape Swedish"=cohen.kappa(dfS[,c(9,10)])$confid[1,],
                        "Handshape Turkish"=cohen.kappa(dfT[,c(9,10)])$confid[1,]))
percent=c(100-nrow(df[df$Dimension!=df$DimensionIL,])/nrow(df)*100,
          100-nrow(df[df$Handshape!=df$HandshapeIL,])/nrow(df)*100,
          100-nrow(dfS[dfS$Dimension!=dfS$DimensionIL,])/nrow(dfS)*100,
          100-nrow(dfT[dfT$Dimension!=dfT$DimensionIL,])/nrow(dfT)*100,
          100-nrow(dfS[dfS$Handshape!=dfS$HandshapeIL,])/nrow(dfS)*100,
          100-nrow(dfT[dfT$Handshape!=dfT$HandshapeIL,])/nrow(dfT)*100)
KappaConfint=round(data.frame(KappaConfint, percent),3)
KappaConfint

#How to report: 
#e.g. "There was excellent agreement between coders, κ = .593 (95% CI, .300 to .886), p < .0005."


# data with disagreement
dfDis=df[df$Dimension!=df$DimensionIL | df$Handshape!=df$HandshapeIL,]
dfDis=dfDis[,-c(4:6,13)]
dfDis=subset(dfDis,!is.na(dfDis$File))
DisTab<-xtable(dfDis)
print.xtable(DisTab, type="html", file="DisTab")
