# libraries general
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

################################################ load required dfs if needed

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

################################################
# CHES
# EU_POSITION: overall orientation of the party leadership towards european integration

# ## in 2014
# ggplot(data = ches14_f, aes(eu_position, party_name)) + 
#   geom_point() +
#   ggtitle(label = "Position towards EU integration in 2014")
# 
# ## in 2019
# ggplot(data = ches19_f, aes(eu_position, party)) + 
#   geom_point() + 
#   ggtitle(label = "Position towards EU integration in 2019")
# 
# ggplot(data = ches19_f, aes(antielite_salience, party)) +
#   geom_point()
# 
# ggplot(data = ches19_f, aes(people_vs_elite, party)) +
#   geom_point()

# populism-left/right-matrix

# populism and left right scale: GRC - SYRIZA and ANEL
ggplot(data = poppa_GRC, aes(x = lroverall, y = populism)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lroverall, y = populism), colour = "red", size = 5) +
  ggtitle(label = "SYRIZA und ANEL im Raum zwischen Populismus und Links-Rechts-Verortung", 
          subtitle = "Nach POPPA 2019") +
  xlab(label = "Links-Rechts-Verortung") + 
  ylab(label = "Populismus (ideeller Ansatz)")


# populism and left right scale: ITA - M5S and Lega
ggplot(data = poppa_ITA, aes(x = lroverall, y = populism)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_M5S_LN, aes(x = lroverall, y = populism), colour = "red", size = 5) +
  ggtitle(label = "M5S und Lega Nord im Raum zwischen Populismus und Links-Rechts-Verortung", 
          subtitle = "Nach POPPA 2019") +
  xlab(label = "Links-Rechts-Verortung") + 
  ylab(label = "Populismus (ideeller Ansatz)")

# GRC: testing poppa vars for non congruent party positions of SYRIZA and ANEL

## populist spectrum: ideational approach dimensions (red)

ggplot(data = poppa_GRC, aes(x = populism, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = populism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: Populismus nach ideellem Ansatz",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Populismus",
       y = "Partei")

### antielitism
ggplot(data = poppa_GRC, aes(x = antielitism, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = antielitism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: (Anti-)Elitismus",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "(Anti-)Elitismus",
       y = "Partei")

### manichean worldview
ggplot(data = poppa_GRC, aes(x = manichean, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = manichean, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: Manichäisches Weltbild",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Manichäisches Weltbild",
       y = "Partei")

### indivisible people
ggplot(data = poppa_GRC, aes(x = indivisble, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = indivisble, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: Homogenität des Volks",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Homogenität",
       y = "Partei")

### general will of the people
ggplot(data = poppa_GRC, aes(x = generalwill, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = generalwill, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: Singulärer Volkswille",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Volkswille",
       y = "Partei")

### people centrism
ggplot(data = poppa_GRC, aes(x = peoplecentrism, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = peoplecentrism, y = party), colour = "red", size = 5) +
  labs(title = "SYRIZA und ANEL: Volk = alleiniger Souverän",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Volk = alleiniger Souverän",
       y = "Partei")

## political style of parties (purple)

### emotions as strategy
ggplot(data = poppa_GRC, aes(x = emotional, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = emotional, y = party), colour = "purple", size = 5) +
  labs(title = "SYRIZA und ANEL: Emotionen als Strategie",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Emotionen als Strategie",
       y = "Partei")

### complexity of decision making in politics
ggplot(data = poppa_GRC, aes(x = complex, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = complex, y = party), colour = "purple", size = 5) +
  labs(title = "SYRIZA und ANEL: Komplexität politischer Entscheidungen",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Komplexität politischer Entscheidungen",
       y = "Partei")

########################################### party ideology (green)

### lroverall
ggplot(data = poppa_GRC, aes(x = lroverall, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lroverall, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: politisch Links vs. Rechts",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Links-Rechts (politisch)",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)

### lrecon
ggplot(data = poppa_GRC, aes(x = lrecon, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lrecon, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: wirtschaftlich Links vs. Rechts",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Links-Rechts (wirtschaftlich)",
       y = "Partei")

### immigration
ggplot(data = poppa_GRC, aes(x = immigration, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = immigration, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: Position zur Immigration",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Position zur Immigration",
       y = "Partei")

### eu
ggplot(data = poppa_GRC, aes(x = eu, y = party)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = eu, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: Position zur EU",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Position zur EU",
       y = "Partei")

### nativism
ggplot(data = poppa_GRC, aes(x = nativism, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = nativism, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: Nativismus",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Nativismus",
       y = "Partei")

### laworder
ggplot(data = poppa_GRC, aes(x = laworder, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = laworder, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL",
       subtitle = "Bürgerliche Freiheiten vs. Recht u. Ordnung",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Freiheiten - Recht/Ordnung",
       y = "Partei")

### lifestyle
ggplot(data = poppa_GRC, aes(x = lifestyle, y = party)) +
  geom_point() +
  theme_linedraw() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lifestyle, y = party), colour = "green", size = 5) +
  labs(title = "SYRIZA und ANEL: Auffassung von Lebensstil",
       subtitle = "Traditionell vs. Liberal",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Traditionell - Liberal",
       y = "Partei")

#################################### party organization (blue)

### intra-party democracy
ggplot(data = poppa_GRC, aes(x = intradem, y = party)) +
  geom_point() +
  theme_linedraw() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = intradem, y = party), colour = "blue", size = 5) +
  labs(title = "SYRIZA und ANEL: Basisdemokratie in der Partei",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Basisdemokratie",
       y = "Partei")

### personalised leadership
ggplot(data = poppa_GRC, aes(x = personalised, y = party)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = personalised, y = party), colour = "blue", size = 5) +
  labs(title = "SYRIZA und ANEL: Führung stark personalisiert",
       caption = "Nach POPPA 2019",
       tag = "Abbildung 2",
       x = "Personalisierung",
       y = "Partei") +
  scale_x_binned(limits = c(0, 10), n.breaks = 10, nice.breaks = T)







