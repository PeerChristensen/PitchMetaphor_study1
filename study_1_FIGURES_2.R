# STUDY 1 ANALYSIS: Figures 2
# PEER CHRISTENSEN
# OCTOBER 2017

library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)

#labels <- c(Swedish = "Swedish - HEIGHT", Turkish = "Turkisk - THICKNESS")
labels <- c(Swedish = "Swedish\nHEIGHT", Turkish = "Turkisk\nTHICKNESS")

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
df = df %>% select(-Begin, -Trial)
df=droplevels(df)

df = df %>% filter(Gesture == "1", Language == "Swedish" & Metaphor == "Height" |
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
                            levels=c("Yes", "No", "Mixed"))

# verticality
df = df %>%
  mutate(Vert = factor(case_when(
    Dimension == "vert" & grepl("grip",Handshape) == TRUE ~ "Mixed",
    Dimension == "vert" & Handshape == "flat H" ~ "Yes",
    grepl("grip",Handshape) == TRUE ~ "No"
    )))

# n observations
nrow(df)
# by language
table(df$Language)

#######################################
# CONVERGENCE

df %>% 
  filter(!is.na(Converge)) %>%
  group_by(Language, Participant, Converge) %>%
  summarise(n = n()) %>%
  complete(Converge, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Converge) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Converge,y=Freq, fill = Language)) +
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


######################################################
# Verticality

df %>%
  filter(!is.na(Vert)) %>%
  group_by(Language, Participant, Vert) %>%
  summarise(n = n()) %>%
  complete(Vert, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Vert) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Vert == "Yes") %>%
  ggplot(aes(x=Language,y=Freq)) +
  geom_bar(stat="identity", width = .75,
                      fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_viridis_d(option= "B",begin = .2, end = .7) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("weighted mean proportions") +
  xlab("Convergence") +
  scale_x_discrete("Language",labels=labels)

ggsave("wVertical.png", width = 10, height=10)

######################################################
# Point plots

df %>% 
  filter(!is.na(Converge)) %>%
  group_by(Language, Participant, Converge) %>%
  summarise(n = n()) %>% group_by(Language,Converge) %>% summarise(sum(n))

swe_conv=tibble(a=rep(1,173),b=rep(1,173))
swe_conv %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="green3") +
  theme_void()
ggsave("swe_conv.png",width = 10, height=10)

swe_diverge=tibble(a=rep(1,13),b=rep(1,13))
swe_diverge %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="red3") +
  theme_void()
ggsave("swe_div.png",width = 10, height=10)

swe_mixed=tibble(a=rep(1,26),b=rep(1,26))
swe_mixed %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="yellow3") +
  theme_void()
ggsave("swe_mix.png",width = 10, height=10)

tur_conv=tibble(a=rep(1,94),b=rep(1,94))
tur_conv %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="green3") +
  theme_void()
ggsave("tur_conv.png",width = 10, height=10)

tur_diverge=tibble(a=rep(1,35),b=rep(1,35))
tur_diverge %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="red3") +
  theme_void()
ggsave("tur_div.png",width = 10, height=10)

tur_mixed=tibble(a=rep(1,8),b=rep(1,8))
tur_mixed %>%
  ggplot(aes(a,b)) + geom_jitter(size=15, alpha=.8, colour="yellow3") +
  theme_void()
ggsave("tur_mix.png",width = 10, height=10)