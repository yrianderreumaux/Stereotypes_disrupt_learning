#Simulations for power using the simr package. 

#study2 power analysis. 
#####
library(here)
load(here("Data_and_analyses/data/study2Power.rda"))
Study2Power <- powerSim(Study2.model, fixed("Condition_dumsteal", "z"), seed = 5, nsim = 500, alpha = .05)#calculating observed power for study 1
#####

print("Post Hoc power analysis for Study 2 main effect of condition")
print(Study2Power)



