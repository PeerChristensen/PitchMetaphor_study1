# STUDY 1 ANALYSIS: SPEECH
# PEER CHRISTENSEN
# OCTOBER 2017

library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)

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

# create variable for co-expressivity
# neutral gestures = "NO"
df$Coexpress=factor(ifelse(df$Metaphor=="Height" & df$Dimension=="vert" |
                             df$Metaphor=="Height" & df$Handshape=="flat H" & df$Dimension=="vert" |
                             df$Metaphor=="Height" & df$Handshape=="point" & df$Dimension=="vert" |
                             df$Metaphor=="Thickness" & df$Dimension=="size" |
                             df$Metaphor=="Thickness" & grepl("grip",df$Handshape)==T |
                             df$Metaphor=="Thickness" & df$Dimension=="size" & grepl("grip",df$Handshape)==T,"YES","NO"))

# create variable for incongruence
# neutral gesture = "NO"
df$incongruence=ifelse(df$Metaphor=="Height" & df$Dimension=="size" |
                             df$Metaphor=="Height" & grepl("grip",df$Handshape)==T |
                             df$Metaphor=="Height" & df$Dimension=="size" & grepl("grip",df$Handshape)==T |
                             df$Metaphor=="Thickness" & df$Dimension=="vert" |
                             df$Metaphor=="Thickness" & df$Handshape == "flat H" |
                             df$Metaphor=="Thickness" & df$Dimension=="vert" & df$Handshape == "flat H","YES","NO")


df$incongruence[df$incongruence=="YES" & df$Coexpress=="YES"] = "MIXED"
df$incongruence=factor(df$incongruence)

# Combine co-expressivity and incongruence
# 4 levels: YES, NO, MIXED, NEUTRAL
df = df %>% mutate(Coexpress_full = factor(case_when(Coexpress == "NO" & incongruence == "NO" ~ "Neutral",
                                          Coexpress == "NO" & incongruence == "YES" ~ "No",
                                          Coexpress == "YES" & incongruence == "NO" ~ "Yes",
                                          incongruence == "MIXED" ~ "Mixed")))

# n observations
nrow(df)
# by language
table(df$Language)

######################################################
# WORD FREQUENCY BY METAPHOR

word_counts = df %>%
  dplyr::count(Word, Metaphor, Language, sort = T) %>%
  #ungroup() %>%
  group_by(Language) %>%
  top_n(5,n) %>%
  ungroup() %>%
  arrange(Language, -n) %>%
  dplyr::mutate(order = rev(row_number())) 

word_counts %>%
  ggplot(aes(x = order, y = n, fill = Metaphor)) + 
  geom_col(alpha=.9, width = .9) +
  scale_x_continuous(
    breaks = word_counts$order,
    labels = word_counts$Word) +
  facet_wrap(~Language,scales = "free") +
  labs(x = "words", y = "frequency") +
  my_theme() + 
  coord_flip(ylim=c(0,230)) +
  scale_fill_viridis_d(option= "B",begin = .2, end = .7) +
  theme(legend.text = element_text(size=20))
  #labs(title = "Word frequency grouped by metaphor")

ggsave("WordFrequency.png", width = 15, height=10)

######################################################
# METAPHOR FREQUENCY BY LANGUAGE 
df$Metaphor <- factor(df$Metaphor, 
                   levels=c("Height", "Brightness", "Thickness", "Other"))
# WEIGHTED
df %>%
  group_by(Language, Participant, Metaphor) %>%
  summarise(n=n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Metaphor"),summarise,
        Freq = weighted.mean(freq,wt),
        se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Freq > 0.02) %>%
  ggplot(aes(x=Metaphor,y=Freq)) +
  geom_bar(stat="identity", fill = viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, scales = "free_x") +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech metaphors") +
  ylab("Weighted mean proportions") +
  xlab("Metaphors")

ggsave("wSpeech.png", width = 10, height=10)

#UNWEIGHTED   
df %>%
  group_by(Language, Participant, Metaphor) %>%
  summarise(n=n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  plyr::ddply(c("Language","Metaphor"),summarise,
        Freq = mean(freq),
        se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>% 
  filter(Freq > 0.02) %>%
  ggplot(aes(x=Metaphor,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, scales = "free_x") +
  my_theme() +
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech metaphors") +
  ylab("Mean proportions")  +
  xlab("Metaphors")

ggsave("Speech.png", width = 10, height=10)

######################################################
# GESTURE FREQUENCY WITH LANGUAGE-SPECIFIC SPATIAL METAPHORS
labels <- c(Swedish = "Swedish - HEIGHT", Turkish = "Turkisk - THICKNESS")
levels(df$Gesture) = c("NO", "YES")

# weighted
df %>% 
  filter(Gesture != "NA",Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Gesture) %>%
  summarise(n = n()) %>%
  complete(Gesture, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Gesture"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Gesture,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() +
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Gesture frequency with language-specific spatial metaphors") +
  ylab("Weighted mean proportions") +
  xlab("Gesture")

ggsave("wGestureFrequency.png", width = 10, height=10)

# unweighted
df %>% 
  filter(Gesture != "NA",Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Gesture) %>%
  summarise(n = n()) %>%
  complete(Gesture, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  plyr::ddply(c("Language","Gesture"),summarise,
              Freq = mean(freq),
              se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Gesture,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Gesture frequency with language-specific spatial metaphors") +
  ylab("mean proportions") +
  xlab("Gesture")

ggsave("GestureFrequency.png", width = 10, height=10)

######################################################
# NOT USED  !!!
# FREQUENCY OF CO-EXPRESSIVE GESTURES WITH LANGUAGE-SPECIFIC SPATIAL METAPHORS

# weighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress) %>%
  summarise(n = n()) %>%
  complete(Coexpress, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Coexpress"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Coexpress,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture coexpressivity") +
  ylab("Weighted mean proportions") +
  xlab("Co-expressivity")

ggsave("wCoexpressivity.png", width = 10, height=10)

# unweighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress) %>%
  summarise(n = n()) %>%
  complete(Coexpress, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  plyr::ddply(c("Language","Coexpress"),summarise,
              Freq = mean(freq),
              se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Coexpress,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture co-expressivity") +
  ylab("mean proportions") +
  xlab("Co-expressivity")

ggsave("Coexpressivity.png", width = 10, height=10)


######################################################
# NOT USED !!!
# FREQUENCY OF INCONGRUENT GESTURES WITH LANGUAGE-SPECIFIC SPATIAL METAPHORS

df$incongruence <- factor(df$incongruence,levels = c("NO","YES","MIXED"))

# weighted
df %>% 
  filter(Gesture == "YES", !is.na(incongruence), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, incongruence) %>%
  summarise(n = n()) %>%
  complete(incongruence, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","incongruence"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=incongruence,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(breaks = c("NO","YES","MIXED")) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("Weighted mean proportions") +
  xlab("Incongruence")

ggsave("wIncongruence.png", width = 10, height=10)

# unweighted
df %>% 
  filter(Gesture == "YES", !is.na(incongruence), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, incongruence) %>%
  summarise(n = n()) %>%
  complete(incongruence, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  plyr::ddply(c("Language","incongruence"),summarise,
              Freq = mean(freq),
              se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=incongruence,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("mean proportions") +
  xlab("Incongruence")

ggsave("Incongruence.png", width = 10, height=10)

######################################################
# COMBINED CONVERGENCE AND DIVERGENCE
labels2 = c("Swedish - HEIGHT", "Turkish - THICKNESS")
df$Coexpress_full <- factor(df$Coexpress_full, 
                      levels=c("Yes", "No", "Mixed"))
# weighted
df %>% 
  filter(Gesture == "YES",!is.na(Coexpress_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress_full) %>%
  summarise(n = n()) %>%
  complete(Coexpress_full, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Coexpress_full) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  #plyr::ddply(c("Language","Coexpress_full"),summarise,
  #            Freq = weighted.mean(freq,wt),
  #            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Coexpress_full,y=Freq, fill = Language)) +
  geom_bar(position=position_dodge(.9),stat="identity") +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2,position=position_dodge(.9)) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  theme(legend.text = element_text(size=20),
        legend.key.width = unit(2.5, "lines"),
        legend.key.height = unit(2.5, "lines")) +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_viridis_d(option= "B",begin = .2, end = .7) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("weighted mean proportions") +
  xlab("Convergence")
  
ggsave("wConvergenceFull.png", width = 10, height=10)


#VERSION 2

# weighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress_full) %>%
  summarise(n = n()) %>%
  complete(Coexpress_full, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Coexpress_full"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Coexpress_full,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("weighted mean proportions") +
  xlab("Co-expressivity")

# unweighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress_full) %>%
  summarise(n = n()) %>%
  complete(Coexpress_full, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n)) %>% 
  plyr::ddply(c("Language","Coexpress_full"),summarise,
              Freq = mean(freq),
              se = sqrt(sd(freq))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Coexpress_full,y=Freq)) +
  geom_bar(stat="identity", fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("mean proportions") +
  xlab("Incongruence")

######################################################
labels <- c(Swedish = "Swedish\nHEIGHT", Turkish = "Turkisk\nTHICKNESS")
# Verticality
df = df %>% 
  mutate(Vert = factor(case_when(
    Dimension == "vert" & Handshape == "flat H" ~ "YES",
    TRUE ~ "other")))
  
df %>% 
  filter(Gesture == "YES", Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Vert) %>%
  summarise(n = n()) %>%
  complete(Vert, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language,Vert) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Vert=="YES")


# Flat hand

######################################################
# SIMPLE CONVERGENCE
labels <- c(Swedish = "Swedish\nHEIGHT", Turkish = "Turkisk\nTHICKNESS")

# weighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress_full) %>%
  summarise(n = n()) %>%
  complete(Coexpress_full, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Coexpress_full"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Coexpress_full == "Yes") %>%
  ggplot(aes(x=Language,y=Freq)) +
  geom_bar(stat="identity", width = .75,
           fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture coexpressivity") +
  ylab("Weighted mean proportions") +
  scale_x_discrete("Language",labels=labels)

ggsave("wConvergence.png", width = 10, height=10)

######################################################
# SIMPLE DIVERGENCE

# weighted
df %>% 
  filter(Gesture == "YES", !is.na(Coexpress), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, incongruence) %>%
  summarise(n = n()) %>%
  complete(incongruence, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","incongruence"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(incongruence=="YES") %>%
  ggplot(aes(x=Language,y=Freq)) +
  geom_bar(stat="identity", width = .75,
           fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture coexpressivity") +
  ylab("Weighted mean proportions") +
  scale_x_discrete("Language",labels=labels)

ggsave("wDivergence.png", width = 10, height=10)

######################################################
# Point plots

df %>% 
  filter(Gesture == "YES", !is.na(Coexpress_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") %>%
  group_by(Language, Participant, Coexpress_full) %>%
  summarise(n = n()) %>% group_by(Language,Coexpress_full) %>% summarise(sum(n))

swe_conv=tibble(a=rep(1,216),b=rep(1,216))
swe_conv %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="green3") +
  theme_void()
ggsave("swe_conv.png",width = 10, height=10)

swe_diverge=tibble(a=rep(1,13),b=rep(1,13))
swe_diverge %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="red3") +
  theme_void()
ggsave("swe_div.png",width = 10, height=10)

swe_mixed=tibble(a=rep(1,26),b=rep(1,26))
swe_mixed %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="yellow3") +
  theme_void()
ggsave("swe_mix.png",width = 10, height=10)

tur_conv=tibble(a=rep(1,98),b=rep(1,98))
tur_conv %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="green3") +
  theme_void()
ggsave("tur_conv.png",width = 10, height=10)

tur_diverge=tibble(a=rep(1,62),b=rep(1,62))
tur_diverge %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="red3") +
  theme_void()
ggsave("tur_div.png",width = 10, height=10)

tur_mixed=tibble(a=rep(1,8),b=rep(1,8))
tur_mixed %>%
  ggplot(aes(a,b)) + geom_jitter(size=10, alpha=.8, colour="yellow3") +
  theme_void()
ggsave("tur_mix.png",width = 10, height=10)

#####################################################
# WORD CLOUD
library(wordcloud2)
library(plyr)

word_counts = df %>%
  dplyr::count(Word, sort = T)

word_counts$Word=revalue(word_counts$Word, c("ljusare"="bright","lägre"="low","djupa"="deep",
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


wordcloud2(word_counts,figPath = "gkey.png",
           color=viridis_pal(option="B", begin = .2, end =.8)(25),
           size=4,
           maxRotation = pi)



