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

################################################ load required dfs if needed

# ches 19
ches19 <- read_dta(file = "/Users/flo/Desktop/data/ches/19/CHES2019V3.dta")
#
ches19_f <- ches19 %>% filter(country == 4 | country == 8)
ches19_GRC <- ches19 %>% filter(country == 4)
ches19_ITA <- ches19 %>% filter(country == 8)
#
ches19_M5S_LN <- ches19_ITA[c(1, 8),]

# # ches 17
# ches17 <- read_dta(file = "/Users/flo/Desktop/data/ches/17/CHES_means_2017.dta")
# #
# ches17_f <- ches17 %>% filter(country == 4 | country == 8)

# ches 14
ches14 <- read_dta(file = "/Users/flo/Desktop/data/ches/14/2014_CHES_dataset_means-2.dta")
#
ches14_f <- ches14 %>% filter(country == 4 | country == 8)
ches14_GRC <- ches14 %>% filter(country == 4)
ches14_ITA <- ches14 %>% filter(country == 8)
#
ches14_SYRIZA_ANEL <- ches14_GRC[c(3, 6),]

# poppa expert survey
poppa <- read_dta(file = "/Users/flo/Desktop/data/poppa/party_means.dta")
#
poppa_f <- poppa %>% filter(country_id == 13 | country_id == 16)
poppa_GRC <- poppa %>% filter(country_id == 13)
poppa_ITA <- poppa %>% filter(country_id == 16)
#
poppa_SYRIZA_ANEL <- poppa_GRC[c(1, 7),]
poppa_M5S_LN <- poppa_ITA[c(2, 4),]

# elnes
# elnes <- read_sav(file = "/Users/flo/Desktop/data/elnes/ELNES-2015b-clean-weights.sav")

################################################

# EU_POSITION: overall orientation of the party leadership towards european integration

## in 2014
ggplot(data = ches14_f, aes(eu_position, party_name)) + 
  geom_point() +
  ggtitle(label = "Position towards EU integration in 2014")

## in 2019
ggplot(data = ches19_f, aes(eu_position, party)) + 
  geom_point() + 
  ggtitle(label = "Position towards EU integration in 2019")

ggplot(data = ches19_f, aes(antielite_salience, party)) +
  geom_point()

ggplot(data = ches19_f, aes(people_vs_elite, party)) +
  geom_point()

# ideational for poppa expert survey
## manichean ww
## indivisible people
## general will
## people-centrism
## anti-elitism

ideational <- poppa_f %>% subset(select = c("manichean", "indivisble", "generalwill", "peoplecentrism", "antielitism"))

cor_idea <- cor(ideational)
efa_ideational <- fa(cor_idea, rotate = "varimax")
summary(efa_ideational)
str(efa_ideational)
# loadings of vars in factor
efa_ideational$loadings
# uniquenesses of vars in factor
efa_ideational$uniquenesses
# values of factors
efa_ideational$values

pca_ideational <- prcomp(ideational)
summary(pca_ideational)


ggplot(data = poppa_GRC, aes(x = lroverall, y = populism)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5)

ggplot(data = poppa_ITA, aes(x = lroverall, y = populism)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5)




ggplot(data = poppa_GRC, aes(x = lroverall, y = populism)) +
  geom_point() +
  geom_text(aes(label = party), hjust = 0.5, vjust = 1.5) +
  geom_point(data = poppa_SYRIZA_ANEL, aes(x = lroverall, y = populism), colour = "red", size = 5) +
  ggtitle(label = "SYRIZA und ANEL im Raum zwischen Populismus und Links-Rechts-Verortung", 
          subtitle = "Nach POPPA 2019") +
  xlab(label = "Links-Rechts-Verortung") + 
  ylab(label = "Populismus (nach ideellem Ansatz)")









