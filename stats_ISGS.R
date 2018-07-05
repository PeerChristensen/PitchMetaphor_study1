# stats ISGS


library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)
library(lme4)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

df = read_csv2("all_data_CLEAN.csv")
df = df %>% select(Participant = File, 
                   Begin = Begin.Time...hh.mm.ss.ms, 
                   Language, 
                   Trial, 
                   Metaphor = Scope, 
                   Word = Words, 
                   Gesture, 
                   Dimension,
                   Handshape)
df=df[!duplicated(df), ]
df$Word = factor(df$Word)
df$Metaphor = factor(df$Metaphor)
df$Gesture = factor(df$Gesture)
df$Language = factor(df$Language)
df = df %>% select(-Begin, -Trial)
df=droplevels(df)

# VARIABLE FOR LANGUAGE-SPECIFIC METAPHOR USE
df %<>% mutate(ls_Meta = factor(case_when(
    Language == "Swedish" & Metaphor == "Height" |
    Language == "Turkish" & Metaphor == "Thickness" ~ 1,
    TRUE ~ 0)))


############################################
# SPEECH : Language-specific spatial metqphor ~ Language

fit1 = glmer(ls_Meta ~ Language + (1|Participant),data=df, family="binomial")
summary(fit1)
tidy(fit1)
confint(fit1)
logit2prob(summary(fit1)$coef)
plogis(summary(fit1)$coef)
exp(summary(fit1)$coef)

############################################
# Convergence

df %<>% filter(Gesture == "1", Language == "Swedish" & Metaphor == "Height" |
                 Language == "Turkish" & Metaphor == "Thickness")

# create variable for convergence
df = df %>%
  mutate(Converge = factor(case_when(
    
    Metaphor=="Height" & Dimension=="vert" & grepl("grip",Handshape)==T |
      Metaphor=="Thickness" & Dimension=="vert" & grepl("grip",Handshape)==T ~ "Mixed",
    
    Metaphor=="Height" & grepl("grip",Handshape)==T |
      Metaphor=="Thickness" & Dimension=="vert" & Handshape == "flat H" ~ "No",
    
    Metaphor=="Height" & Dimension =="vert" & Handshape=="flat H" |
      Metaphor=="Thickness" & grepl("grip",Handshape)==T ~ "Yes"
  )))
df$Converge <- factor(df$Converge, 
                      levels=c("No", "Yes", "Mixed"))
df$Language <- factor(df$Language, 
                      levels=c("Turkish", "Swedish"))

df %<>% filter(Converge != "Mixed")
df = droplevels(df)

fit2 = glmer(Converge ~ Language + (1|Participant),data=df, family = binomial)
summary(fit2)
tidy(fit2)
confint(fit2)
logit2prob(summary(fit2)$coef)
plogis(summary(fit2)$coef)
exp(summary(fit2)$coef)

########################################
# Verticality
df = df %>%
  mutate(Vert = factor(case_when(
    Dimension == "vert" & Handshape == "flat H" ~ "Yes",
    grepl("grip",Handshape) == TRUE ~ "No"
  )))

fit3 = glmer(Vert ~ Language + (1|Participant),data=df, family = binomial)
summary(fit3)
tidy(fit3)
confint(fit3)
logit2prob(summary(fit3)$coef)
plogis(summary(fit3)$coef)
exp(summary(fit3)$coef)




