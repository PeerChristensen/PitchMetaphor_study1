# speech-gesture convergence, redo


library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)
library(lme4)

my_theme <- function() {
  theme_apa() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_blank()) +
    theme(panel.border = element_blank()) +                       # facet border
    theme(strip.background = element_blank()) +                  # facet title background
    theme(strip.text.x = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20,margin = margin(t = 25, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(size = 20,margin = margin(t = 0, r = 25, b = 0, l = 0))) +
    theme(axis.text.y = element_text(size = 20)) +
    theme(axis.text.x =  element_text(size = 20)) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}


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

############################################
# Convergence

#df %<>% filter(Gesture == "1", Language == "Swedish" & Metaphor == "Height" |
#                 Language == "Turkish" & Metaphor == "Thickness")

df %<>% filter(Gesture == "1", Language == "Swedish" & Metaphor == "Height" |
                 Language == "Turkish" & Metaphor == "Thickness")
# Participants produced 640 cases with gesture and lang-spc. metaphor, 
#322 swedish, 318 turkish

# create variable for convergence
df <- df %>%
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
                      levels=c("Swedish", "Turkish"))

#df %<>% filter(Converge != "Mixed")
df = droplevels(df)
df <- df %>% filter(!is.na(Converge))

table(df$Language,df$Converge)
# 349 cases with representational gestures
#         No Yes Mixed
#Swedish  13 173    26
#Turkish  35  94     8

# weighted

df$Converge <- factor(df$Converge, 
                      levels=c("Yes", "No", "Mixed"))

df %>%
  group_by(Language, Participant, Converge) %>%
  summarise(n = n()) %>%
  complete(Converge, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Converge"),summarise,
              n=sum(n),
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  #filter(Converge == "Yes") %>%
  ggplot(aes(x=Converge,y=Freq,fill=Language)) +
  geom_bar(stat="identity", width = .75,position="dodge")+
  scale_fill_viridis_d(option = "B", begin = .2, end = .7, direction = -1) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2,position=position_dodge(.75)) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture coexpressivity") +
  ylab("Weighted mean proportions") +
  scale_x_discrete("Language",labels=labels)

# n obs
table(df$Language)

# regression
df$Language <- factor(df$Language, 
                      levels=c("Turkish", "Swedish"))

df <- df %>% filter(Converge == "Yes" | Converge == "No")
df = droplevels(df)

fit2 = glmer(Converge ~ Language + (1|Participant),data=df, family = binomial)
summary(fit2)
tidy(fit2)
confint(fit2)
logit2prob(summary(fit2)$coef)
plogis(summary(fit2)$coef)
exp(summary(fit2)$coef)
