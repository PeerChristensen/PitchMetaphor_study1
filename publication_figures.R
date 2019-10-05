# publication figures
# october 2019

# min. 300 ppi
# pdf
# All ordinate and abscissa quantities should be in 9-point font.
# Helvetica Neue 57 Condensed roman font
# All main ordinate and abscissa labels should be in 10-point font.
# The title header (at the top of a figure), if there is one, should be in 12-point font.
# Keys should be in 9-point font. 
# Title case


# STUDY 1 ANALYSIS: SPEECH
# PEER CHRISTENSEN
# OCTOBER 2017

library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)
library(ggthemes)
library(ggpubr)
library(extrafont)

helv_font <- "HelveticaNeueLT Std Cn"

my_theme <- function() {
  theme_apa() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_blank()) +
    theme(panel.border = element_blank()) +    
    theme(panel.border = element_blank()) +                       # facet border
    theme(strip.background = element_blank()) +                  # facet title background
    theme(strip.text.x = element_text(family = helv_font,size = 10)) +
    theme(title        = element_text(family = helv_font,size = 12,margin = margin(t = 25, r = 0, b = 25, l = 0))) +
    theme(axis.title.x = element_text(family = helv_font,size = 10,margin = margin(t = 25, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(family = helv_font,size = 10,margin = margin(t = 0, r = 25, b = 0, l = 0))) +
    theme(axis.text.y = element_text(family  = helv_font,size = 9)) +
    theme(axis.text.x =  element_text(family = helv_font,size = 9,margin = margin(t=-10))) +
    theme(axis.ticks = element_blank()) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

df <- read_csv2("all_data_CLEAN.csv") %>%
  select(Participant = File, 
                   Begin = Begin.Time...hh.mm.ss.ms, 
                   Language, 
                   Trial, 
                   Metaphor = Scope, 
                   Word     = Words, 
                   Gesture, 
                   Dimension,
                   Handshape)

df <- df[!duplicated(df), ]

df$Word     = factor(df$Word)
df$Metaphor = factor(df$Metaphor)
df$Gesture  = factor(df$Gesture)
df$Language = factor(df$Language)


######################################
#### Figure 1: SPEECH ################
######################################

df$Metaphor <- factor(df$Metaphor, 
                      levels=c("Height", "Brightness", "Thickness", "Other"))

df %>%
  group_by(Language, Participant, Metaphor) %>%
  dplyr::summarise(n=n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  dplyr::mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Metaphor"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Freq > 0.02) %>%
  ggplot(aes(x=Metaphor,y=Freq)) +
  geom_bar(stat="identity", fill = viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1),width=.8) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2,size=.5) +
  facet_wrap(~Language, scales = "free_x") +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  ylab("Weighted Mean Proportions") +
  xlab("Metaphors")
ggsave("figure1.pdf",device=cairo_pdf)


######################################
#### Figure 2: SPEECH & GESTURE ######
######################################

# create variables for convergence
#####
# neutral gestures = "NO"
df <- df %>% 
  filter(Gesture==1,
                    (Language == "Swedish" & Metaphor == "Height") |
                    (Language == "Turkish" & Metaphor == "Thickness")) %>%
  select(-Begin,-Trial,-Gesture)

df$Convergence=factor(ifelse(df$Metaphor=="Height" & df$Dimension=="vert" |
                             df$Metaphor=="Height" & df$Handshape=="flat H" & df$Dimension=="vert" |
                             df$Metaphor=="Height" & df$Handshape=="point" & df$Dimension=="vert" |
                             df$Metaphor=="Thickness" & df$Dimension=="size" |
                             df$Metaphor=="Thickness" & grepl("grip",df$Handshape)==T |
                             df$Metaphor=="Thickness" & df$Dimension=="size" & grepl("grip",df$Handshape)==T,"Convergence","No"))

# create variable for divergence
# neutral gesture = "NO"
df$Divergence=ifelse(df$Metaphor=="Height" & df$Dimension=="size" |
                         df$Metaphor=="Height" & grepl("grip",df$Handshape)==T |
                         df$Metaphor=="Height" & df$Dimension=="size" & grepl("grip",df$Handshape)==T |
                         df$Metaphor=="Thickness" & df$Dimension=="vert" |
                         df$Metaphor=="Thickness" & df$Handshape == "flat H" |
                         df$Metaphor=="Thickness" & df$Dimension=="vert" & df$Handshape == "flat H","Divergence","No")


df$Divergence[df$Divergence=="Divergence" & df$Convergence=="Convergence"] = "MIXED"
df$Divergence=factor(df$Divergence)

# Combine co-expressivity and incongruence
# 4 levels: YES, NO, MIXED, NEUTRAL
df <- df %>% 
  dplyr::mutate(Convergence_full = factor(case_when(Convergence == "No" & Divergence == "No" ~ "Neutral",
                                                    Convergence == "No" & Divergence == "Divergence" ~ "Divergence",
                                                    Convergence == "Convergence" & Divergence == "No" ~ "Convergence",
                                                    Divergence  == "MIXED" ~ "Mixed")))

labels2 = c("Swedish - Height", "Turkish - Thickness")
df$Convergence_full <- factor(df$Convergence_full, 
                            levels=c("Convergence", "Divergence"))

#####

df <- df %>% 
  filter(!is.na(Convergence_full), Language == "Swedish" & Metaphor == "Height" |
           Language == "Turkish" & Metaphor == "Thickness") 

table(df$Language,df$Convergence_full)

df %>%
  mutate(Language = ifelse(Language == "Swedish","Swedish - Height", "Turkish - Thickness")) %>%
  group_by(Language, Participant, Convergence_full) %>%
  dplyr::summarise(n = n()) %>%
  complete(Convergence_full, nesting(Participant), fill = list(n = 0)) %>%
  dplyr::mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Convergence_full) %>%
  dplyr::summarise(Freq = weighted.mean(freq,wt),
                   se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  ggplot(aes(x=Convergence_full,y=Freq, fill = Language)) +
  geom_bar(position=position_dodge(.9),stat="identity",width=.8) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2,position=position_dodge(.9),size=.5) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
   theme(legend.text = element_text(size=9),
         legend.key.width = unit(1.5, "lines"),
         legend.spacing = unit(7,"lines"),
         legend.key.height = unit(1.5, "lines")) +
  theme(legend.position = "top") +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_viridis_d(option= "B",begin = .2, end = .7) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("Weighted Mean Proportions") +
  xlab("Speech-Gesture Relationship")

ggsave("figure2.pdf",device=cairo_pdf)

