# FINAL ANAlYSIS MUSIC EXP DATA: REGRESSION MODELS
# PEER CHRISTENSEN
# OCTOBER 2017
library(lme4)
library(lmerTest)

setwd("/Users/peerchristensen/Desktop")
df_full= read.csv2("all_data_CLEAN.csv",na.strings = c(""),stringsAsFactors = T)
df=df_full[,-c(1,3,13,14,15,16,17,18)]
df=df[!duplicated(df), ]
df$Express=ifelse(df$Scope=="Height" & df$Dimension=="vert" |
                     df$Scope=="Height" & df$Handshape=="flat H" |
                     df$Scope=="Height" & df$Handshape=="point" |
                     df$Scope=="Thickness" & df$Dimension=="size" |
                     df$Scope=="Thickness" & grepl("grip",df$Handshape)==T,1,0)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#### 1.Gesture production ###
# 1a. Overall
mod1a =glmer(Gesture~Language+(1|File),data=df,family="binomial")
summary(mod1a)
#log odds to odds
exp(summary(mod1a)$coefficients[,1])
#probabilities
logit2prob(coef(summary(mod1a))[,1])

# 1b. With language-specific metaphors
df1=subset(df,Language=="Swedish" & Scope=="Height" |
             Language=="Turkish" & Scope=="Thickness")
df1=droplevels(df1)
mod1b =glmer(Gesture~Language+(1|File),data=df1,family="binomial")
summary(mod1b)
#log odds to odds
exp(summary(mod1b)$coefficients[,1])
#probabilities
logit2prob(coef(summary(mod1b))[,1])

# ####2. Speech-gesture co-expressivity with language-specific metahpors ###
df2=subset(df,Language=="Swedish" & Scope=="Height" |
             Language=="Turkish" & Scope=="Thickness")
df2=droplevels(df2)
df2$Scope=relevel(df2$Scope,ref="Thickness")

mod2=glmer(Express~Scope+(1|File),data=df2,family="binomial")
summary(mod2)
exp(summary(mod2)$coefficients[,1])
logit2prob(coef(summary(mod2))[,1])

# 3. Speech-gesture co-expressivity with HEIGHT
df3=subset(df,Scope=="Height")

mod3=glmer(Express~Language+(1|File),data=df3,family="binomial")
summary(mod3)
exp(summary(mod3)$coefficients[,1])
logit2prob(coef(summary(mod3))[,1])

# 4. Consistency over time
df4=df
df4$Cons=0
df4$Cons[df4$Language=="Swedish" & df4$Scope=="Height"]=1
df4$Cons[df4$Language=="Turkish" & df4$Scope=="Thickness"]=1

#4.a Swedish
df4a=df4[df4$Language=="Swedish",]

mod4a=glmer(Cons~Trial+(1|File),data=df4a,family="binomial")
exp(summary(mod4a)$coefficients[,1])
logit2prob(coef(summary(mod4a))[,1])

#4.b Turkish
df4b=df4[df4$Language=="Turkish",]

mod4b=glmer(Cons~Trial+(1|File),data=df4b,family="binomial")
exp(summary(mod4a)$coefficients[,1])
logit2prob(coef(summary(mod4b))[,1])

# 5. Likelihood of vertical gestures with other metaphors
df5=df
df5$Vertical=ifelse(df5$Dimension=="vert" | 
                       df5$Handshape=="flat H" |
                       df5$Dimension=="vert" & df5$Handshape=="point",1,0)
df5=subset(df5, Language=="Swedish" & Scope=="Brightness" |
              Language=="Turkish" & Scope=="Thickness")
df5$Scope=relevel(df5$Scope,ref="Thickness")

mod5=glmer(Vertical~Scope+(1|File),data=df5,family="binomial")
exp(summary(mod5)$coefficients[,1])
logit2prob(coef(summary(mod5))[,1])
