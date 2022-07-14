#required packages
#we're using groundog for source management: https://groundhogr.com/using/
library("groundhog")
pkgs <- c("corrplot", "tidyverse", "lme4", "sjPlot", "psych", "MASS",  "bayestestR",
          "lmerTest", "pbkrtest", "emmeans", "RColorBrewer","wesanderson", "simr", "performance",  "brms", "here")
groundhog.library(pkgs, '2022-01-01')

