#required packages
#we're using groundog for source management: https://groundhogr.com/using/
install.packages("groundhog")
library("groundhog")
pkgs <- c("corrplot", "tidyverse", "lme4", "sjPlot", "psych", "MASS",  "bayestestR",
          "lmerTest", "pbkrtest", "emmeans", "RColorBrewer","wesanderson", "simr", "performance",  "brms")
groundhog.library(pkgs, '2022-04-01')

