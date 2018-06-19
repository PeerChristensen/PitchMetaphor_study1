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
  mutate(order = rev(row_number())) 

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
  #ggtitle("Gesture frequency with language-specific spatial metaphors") +
  ylab("mean proportions") +
  xlab("Gesture")

ggsave("GestureFrequency.png", width = 10, height=10)

######################################################
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
  #ggtitle("Speech-gesture co-expressivity") +
  ylab("mean proportions") +
  xlab("Co-expressivity")

ggsave("Coexpressivity.png", width = 10, height=10)


######################################################
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
  #ggtitle("Speech-gesture incongruence") +
  ylab("mean proportions") +
  xlab("Incongruence")

ggsave("Incongruence.png", width = 10, height=10)


