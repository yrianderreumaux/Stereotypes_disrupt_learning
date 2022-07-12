#Simulations for power using the simr package. 

#study 1 power analysis. 
#####
load("/Users/yrianderreumaux/My Drive/Research Projects/WPT /Stereotypes_Disrupt_Learning_Manuscript_R&R_0410_2022/Data_and_analyses/data/study1model.rda")
summary(Study1Power)
mod1PostPower <- powerSim(Study1Power, fixed("Samp_GroupB_dumIn.Group", "z"), seed = 5, nsim = 800, alpha = .05)#calculating observed power for study 1
extend <- extend(Study1Power, along="Participant", n=1000) 
mod1PostPower2 <- powerSim(extend, fixed("Samp_GroupB_dumIn.Group", "z"), seed = 2, nsim = 800, alpha = .05)
sim2.power.curve <- powerCurve(Study1Power, test = fixed("Samp_GroupB_dumIn.Group", "z"), along ="Participant" , nsim=800) #run power curve. Takes a long time. 
#####


print("Simulations for power")
print("Study 1")
print(mod1PostPower)
print("Study 1 with 1k participants")
print(mod1PostPower2)




