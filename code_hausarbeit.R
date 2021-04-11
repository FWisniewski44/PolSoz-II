###################################################
######## CODE POLSOZ II (WINTER 2020-21) ##########
############# PROF. DR. NILS STEINER ##############
###################################################
############## FLORIAN WISNIEWSKI #################
####### STUDENT M. A. POLITIKWISSENSCHAFT #########
###################################################

# Jeglicher Code, der für diese Hausarbeit geschrieben wurde, ist im GitHub-Repository für diese Arbeit enthalten.
# Diese Datei listet zudem erneut die endgültig in der Arbeit verwendeten Code-Snippets auf.
# Ebenso sind diese aus der RMarkdown-Datei "HA_word.Rmd" ersichtlich.

################################################### Verwendete Packages

library(tidymodels)
library(tidyverse)
library(rms)
library(MASS)
library(corrplot)
library(lubridate)
library(Amelia)
library(xtable)
library(haven)
library(foreign)
library(broom)
library(psych)
library(Hmisc)
library(expss)
library(rockchalk)
library(effects)
library(modeldata)
library(randomForest)
library(compare)
library(psych)
library(desc)
library(PerformanceAnalytics)
library(reshape2)
library(d3heatmap)
library(kableExtra)
library(gtools)
library(gmodels)
library(summarytools)
library(data.table)
library(ggpubr)

################################################### Geladene Datensätze
################################################### (Stehen auf Nachfrage selbstverständlich in der verwendeten Version zur Verfügung)


# ches 19
ches19 <- read_dta(file = "/Users/flo/Desktop/data/ches/19/CHES2019V3.dta")

# ches 19 filtered (2 in 1 and separately)
ches19_f <- ches19 %>% filter(country == 4 | country == 8)
ches19_GRC <- ches19 %>% filter(country == 4)
ches19_ITA <- ches19 %>% filter(country == 8)

# ches 19 ITA, only M5S and LN
ches19_M5S_LN <- ches19_ITA[c(1, 8),]

# ches 14
ches14 <- read_dta(file = "/Users/flo/Desktop/data/ches/14/2014_CHES_dataset_means-2.dta")

# ches 14 filtered (2 in 1 and separately)
ches14_f <- ches14 %>% filter(country == 4 | country == 8)
ches14_GRC <- ches14 %>% filter(country == 4)
ches14_ITA <- ches14 %>% filter(country == 8)

# ches 14 GRC, only SYRIZA and ANEL
ches14_SYRIZA_ANEL <- ches14_GRC[c(3, 6),]

# poppa expert survey
poppa <- read_dta(file = "/Users/flo/Desktop/data/poppa/party_means.dta")

# poppa filtered (2 in 1 and separately)
poppa_f <- poppa %>% filter(country_id == 13 | country_id == 16)
poppa_GRC <- poppa %>% filter(country_id == 13)
poppa_ITA <- poppa %>% filter(country_id == 16)

# poppa filtered: ITA = M5S and LN | GRC = SYRIZA and ANEL
poppa_SYRIZA_ANEL <- poppa_GRC[c(1, 7),]
poppa_M5S_LN <- poppa_ITA[c(2, 4),]

# elnes (should it be required, it can be loaded here)
# elnes <- read_sav(file = "/Users/flo/Desktop/data/elnes/ELNES-2015b-clean-weights.sav")

################################################### In der Arbeit verwendete Graphiken

# Verortung von SYRIZA und ANEL im Links-Rechts-Spektrum und nach "Populismus-Grad"
ggplot(data = poppa_GRC, aes(x = lroverall, y = populism)) +
  geom_point() +
  theme_light() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.8) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lroverall, y = populism), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Verortung der Partei bezüglich Populismus und Links-Rechts-Position",
       tag = "Abbildung 1", 
       caption = "Daten: POPPA 2019", 
       x = "Links-Rechts-Verortung", 
       y = "Populismus (ideeller Ansatz)") + 
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T) +
  scale_y_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "manichean worldview"
ggplot(data = poppa_GRC, aes(x = manichean, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = manichean, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "manichean: 'Manichäische/s Weltbild bzw. Moralvorstellungen'",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 2",
       x = "Manichäisches Weltbild",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "indivisible people"
ggplot(data = poppa_GRC, aes(x = indivisble, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = indivisble, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "indivisible: 'Homogenität des Volkes'",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 3",
       x = "Homogenität",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "general will of the people"
ggplot(data = poppa_GRC, aes(x = generalwill, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = generalwill, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "generalwill: 'Volonté générale bzw. singulärer Volkswille'",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 4",
       x = "Volkswille",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)


# SYRIZA und ANEL: "people centrism"
ggplot(data = poppa_GRC, aes(x = peoplecentrism, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = peoplecentrism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "peoplecentrism: 'Souveränität sollte allein beim Volk liegen'",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 5",
       x = "Volk als alleiniger Souverän",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "antielitism"
ggplot(data = poppa_GRC, aes(x = antielitism, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = antielitism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: (Anti-)Elitismus",
       subtitle = "antielitism: '(Anti-)Elitismus'",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 6",
       x = "(Anti-)Elitismus",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "emotional style"
ggplot(data = poppa_GRC, aes(x = emotional, y = party)) +
  geom_point() +
  theme_light() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = emotional, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Versuch des Aktivierens von Emotionen bei Wählern",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 7",
       x = "Emotionen als Strategie",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "personalised leadership"
ggplot(data = poppa_GRC, aes(x = personalised, y = party)) +
  geom_point() +
  theme_light() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = personalised, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Zuschnitt der Politik der Partei auf die jeweilige Führung",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 8",
       x = "Personalisierungsgrad",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "immigration"
ggplot(data = poppa_GRC, aes(x = immigration, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = immigration, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Position zu Migration",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 9",
       x = "Position zu Migration",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "nativism"
ggplot(data = poppa_GRC, aes(x = nativism, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = nativism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Nativismus",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 10",
       x = "Nativismus",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "eu position"
ggplot(data = poppa_GRC, aes(x = eu, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = eu, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Position zur EU",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 11",
       x = "Position zur EU",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "laworder"
ggplot(data = poppa_GRC, aes(x = laworder, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = laworder, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Bürgerliche Freiheiten vs. Recht und Ordnung",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 12",
       x = "Freiheiten vs. Recht und Ordnung",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

# SYRIZA und ANEL: "lifestyle"
ggplot(data = poppa_GRC, aes(x = lifestyle, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lifestyle, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Idealer Lebensstil lt. Partei",
       caption = "Daten: POPPA 2019",
       tag = "Abbildung 13",
       x = "Traditionell vs. liberal",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)



###################################################
################ END OF DOCUMENT ##################
###################################################