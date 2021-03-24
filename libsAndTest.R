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

# chapel hill expert survey
ches <- read_dta(file = "/Users/flo/Desktop/data/ches/CHES2019V3.dta")

# chapel hilll expert survey - data on level of individual experts
ches_expert <- read_dta(file = "/Users/flo/Desktop/data/ches/CHES2019_experts.dta")






