# FIGURES for presentation at EuroSLA

# STUDY 1 ANALYSIS: Figures 2
# PEER CHRISTENSEN
# OCTOBER 2017


# -----------------------------------------
# LOAD PACKAGES

library(data.table)
library(tidyverse)
library(viridis)
library(Hmisc)
library(tidytext)
library(jtools)
library(forcats)
library(magrittr)
library(lme4)
library(lmerTest)

# -----------------------------------------
# PLOT THEME


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

# -----------------------------------------
# LOAD AND CLEAN DATA

df <- read_csv2("PitchMetaphor2_CLEAN_edit_gesture_2.csv")
df_original <- df # saving original metaphor levels
df %<>% 
  #select(-c(X,X.1,X.2,Time))                     %>% 
  select(-c(X1,Time))                     %>% 
  mutate(Participant = factor(Participant),
         Metaphor    = factor(Metaphor)   ,
         Language    = factor(Language)   ,
         Order       = factor(Order)      ,
         Gesture     = factor(Gesture))     %>%
  mutate(Metaphor = fct_lump(Metaphor))     %>%
  as_tibble()

# variable for language-specific metaphor usage
df$LangSpec=ifelse(df$Metaphor =="Height" & df$Language == "Swedish" |
                     df$Metaphor == "Thickness" & df$Language == "Turkish",1,0)


#labels <- c(Swedish = "Swedish - HEIGHT", Turkish = "Turkisk - THICKNESS")
labels <- c(Swedish = "Swedish\nHEIGHT", Turkish = "Turkisk\nTHICKNESS")


# -----------------------------------------
# SPEECH METAPHORS

df$Metaphor = fct_relevel(df$Metaphor, levels= c("Height", "Brightness", "Thickness", "Other"))

# WEIGHTED
df %>%
  group_by(Language, Participant, Metaphor) %>%
  summarise(n=n()) %>%
  complete(Metaphor, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  plyr::ddply(c("Language","Metaphor"),summarise,
              Freq = weighted.mean(freq,wt),
              se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  mutate(LangSpec = ifelse(Language=="Swedish" & Metaphor =="Height" | Language=="Turkish" & Metaphor =="Thickness",1,0)) %>%
  #filter(Freq > 0.02) %>%
  ggplot(aes(x=Metaphor,y=Freq,fill=LangSpec)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_c(option = "B", begin = .2, end = .7, direction = -1, guide=F) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  facet_wrap(~Language, scales = "free_x") +
  my_theme() + 
  theme(axis.text.x = element_text(angle=45,hjust=1.1,vjust =1.4),
        axis.ticks.x = element_blank()) +
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech metaphors") +
  ylab("Weighted mean proportions") +
  xlab("Metaphors")

ggsave("wSpeech_bilinguals_v2.png", width = 10, height=10)

# -----------------------------------------
# GESTURE

df = df %>% filter(Gesture == 1, LangSpec ==1)
#df = df %>% filter(Gesture == "1", Language == "Swedish" & Metaphor == "Brightness")


# create variable for convergence
df = df %>%
  mutate(Converge = factor(case_when(
    
    Metaphor=="Height" & Dimension=="vert" & grepl("grip",Handshape)==T |
      Metaphor=="Thickness" & Dimension=="vert" & grepl("grip",Handshape)==T ~ "Mixed",
    
    Metaphor=="Height" & grepl("grip",Handshape)==T |
      Metaphor=="Thickness" & Handshape == "flat H" ~ "No",
    Metaphor=="Height" & Dimension=="size" |
      Metaphor=="Thickness" & Dimension=="vert" ~ "No",
    
    Metaphor=="Height" & Dimension =="vert" |
      Metaphor=="Thickness" & Dimension =="size" ~ "Yes",
    Metaphor=="Height" & Handshape=="flat H" |
      Metaphor=="Thickness" & grepl("grip",Handshape)==T ~ "Yes"
  )))

df$Converge <- factor(df$Converge, 
                      levels=c("Yes", "No"))

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

df %<>% filter(!is.na(Converge))

df %>% 
  filter(!is.na(Converge)) %>%
  group_by(Language, Participant, Converge) %>%
  summarise(n = n()) %>%
  complete(Converge, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Converge) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Converge=="Yes") %>% # !!!
  ggplot(aes(x=Language,y=Freq)) +
  geom_bar(position=position_dodge(.9),stat="identity",
           fill = viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2,position=position_dodge(.9)) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  theme(legend.text = element_text(size=20),
        legend.key.width = unit(2.5, "lines"),
        legend.key.height = unit(2.5, "lines")) +
  coord_cartesian(ylim=c(0,1)) +
  #ggtitle("Speech-gesture incongruence") +
  scale_x_discrete("Language & Metaphor",labels=labels) +
  labs(y="weighted mean proportions") +
  NULL

ggsave("wConvergence_biling.png", width = 10, height=10)


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
  scale_x_discrete("Language",labels=labels)

ggsave("wVertical.png", width = 10, height=10)

# VERTICALITY WITH BRIGHTNESS

labels2 <- c(Brightness = "Swedish\nBRIGHTNESS", Height = "Swedish\nHEIGHT", Thickness = "Turkisk\nTHICKNESS")

df %>%
  filter(!is.na(Vert)) %>%
  group_by(Language, Metaphor, Participant, Vert) %>%
  summarise(n = n()) %>%
  complete(Vert, nesting(Participant), fill = list(n = 0)) %>%
  mutate(freq = n / sum(n), wt=sum(n)) %>%
  group_by(Language, Metaphor, Vert) %>%
  summarise(Freq = weighted.mean(freq,wt),
            se = sqrt(wtd.var(freq,wt))/sqrt(length(unique(Participant)))) %>%
  filter(Vert == "Yes") %>%
  ggplot(aes(x=Metaphor,y=Freq)) +
  geom_bar(stat="identity", width = .75,
           fill= viridis_pal(option = "B", begin = .2, end = .7, direction = -1)(1)) +
  geom_errorbar(aes(ymin=Freq-se,ymax=Freq+se),width=.2) +
  #facet_wrap(~Language, labeller=labeller(Language = labels)) +
  my_theme() + 
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_viridis_d(option= "B",begin = .2, end = .7) +
  #ggtitle("Speech-gesture incongruence") +
  ylab("weighted mean proportions") +
  scale_x_discrete("Language and Metaphor",labels=labels2)

ggsave("wVerticalWBright.png", width = 10, height=10)
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

###########################################
# reg mods

# SPEECH
summary(glmer(LangSpec ~ Language + (1|Participant), data=df, family="binomial"))
