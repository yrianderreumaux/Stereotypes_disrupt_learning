#warning! this script will take quite a long time to run due to the amount of models that need to be estimated

#Load data from google drive
id <- "1u60kB6co6zekQz7HKo1qh2sHauzhVbis" # google file ID
CrimeDF <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
#add factor for study covariate
CrimeDF$study[CrimeDF$Condition=="steal"] <- "two"
CrimeDF$study[CrimeDF$Condition=="neg"] <- "three"

#load positively-valenced data to test individual differences
id <- "1yOdUdZXuzAAaP6n7bva92NC1XdtH8QtI" # google file ID
TouchdownDF <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
TouchdownDF$Participant <- as.factor(TouchdownDF$Participant) #make participant factor

#create categorical card weight and congruency variables
#####
CrimeDF$prob[CrimeDF$Pattern == 1] <- .8947
CrimeDF$prob[CrimeDF$Pattern == 2] <- .7778
CrimeDF$prob[CrimeDF$Pattern == 3] <- .9231
CrimeDF$prob[CrimeDF$Pattern == 5] <- .8333
CrimeDF$prob[CrimeDF$Pattern == 6] <- .8947
CrimeDF$prob[CrimeDF$Pattern == 9] <- .55

CrimeDF$prob[CrimeDF$Pattern == 4 ] <- 1-.2222
CrimeDF$prob[CrimeDF$Pattern == 7] <- 1-.1053
CrimeDF$prob[CrimeDF$Pattern == 8 ] <- 1-.1667
CrimeDF$prob[CrimeDF$Pattern == 10 ] <- 1-.07
CrimeDF$prob[CrimeDF$Pattern == 11 ] <- 1-.44
CrimeDF$prob[CrimeDF$Pattern == 12] <- 1-.1053

CrimeDF$prob[CrimeDF$Pattern == 13] <- .5
CrimeDF$prob[CrimeDF$Pattern == 14] <- .5

#also code as absolute deviation from .5
CrimeDF$prob_abs[CrimeDF$Pattern == 1] <- abs(.8947-.5)
CrimeDF$prob_abs[CrimeDF$Pattern == 2] <- abs(.7778-.5)
CrimeDF$prob_abs[CrimeDF$Pattern == 3] <- abs(.9231-.5)
CrimeDF$prob_abs[CrimeDF$Pattern == 5] <- abs(.8333-.5)
CrimeDF$prob_abs[CrimeDF$Pattern == 6] <- abs(.8947-.5)
CrimeDF$prob_abs[CrimeDF$Pattern == 9] <- abs(.55-.5)

CrimeDF$prob_abs[CrimeDF$Pattern == 4 ] <- abs((1-.2222)-(.5))
CrimeDF$prob_abs[CrimeDF$Pattern == 7] <- abs((1-.1053)-(.5))
CrimeDF$prob_abs[CrimeDF$Pattern == 8 ] <- abs((1-.1667)-(.5))
CrimeDF$prob_abs[CrimeDF$Pattern == 10 ] <- abs((1-.07)-(.5))
CrimeDF$prob_abs[CrimeDF$Pattern == 11 ] <- abs((1-.44)-(.5))
CrimeDF$prob_abs[CrimeDF$Pattern == 12] <- abs((1-.1053)-(.5))

CrimeDF$prob_abs[CrimeDF$Pattern == 13] <- .5 - .5
CrimeDF$prob_abs[CrimeDF$Pattern == 14] <- .5 - .5

#create optimal choice variable
CrimeDF$opt[CrimeDF$Pattern == 1] <- 1
CrimeDF$opt[CrimeDF$Pattern == 2] <- 1
CrimeDF$opt[CrimeDF$Pattern == 3] <- 1
CrimeDF$opt[CrimeDF$Pattern == 5] <- 1
CrimeDF$opt[CrimeDF$Pattern == 6] <- 1
CrimeDF$opt[CrimeDF$Pattern == 9] <- 1

CrimeDF$opt[CrimeDF$Pattern == 4 ] <- 0
CrimeDF$opt[CrimeDF$Pattern == 7] <- 0
CrimeDF$opt[CrimeDF$Pattern == 8 ] <- 0
CrimeDF$opt[CrimeDF$Pattern == 10 ] <- 0
CrimeDF$opt[CrimeDF$Pattern == 11 ] <- 0
CrimeDF$opt[CrimeDF$Pattern == 12] <- 0

CrimeDF$opt[CrimeDF$Pattern == 13] <- .5
CrimeDF$opt[CrimeDF$Pattern == 14] <- .5

#create congruency variable
CrimeDF$congruency[CrimeDF$Stimuli == "white" & CrimeDF$Choice == "no_steal"] <- 1 #congruent
CrimeDF$congruency[CrimeDF$Stimuli == "black" & CrimeDF$Choice == "steal"] <- 1 #congruent
CrimeDF$congruency[CrimeDF$Stimuli == "white" & CrimeDF$Choice == "steal"] <- 0 #incongruent
CrimeDF$congruency[CrimeDF$Stimuli == "black" & CrimeDF$Choice == "no_steal"] <- 0 #incongruent
CrimeDF$congruency <- as.factor(CrimeDF$congruency)

TouchdownDF$congruency[TouchdownDF$Stimuli == "white" & TouchdownDF$Choice == "no_touchdown"] <- 1
TouchdownDF$congruency[TouchdownDF$Stimuli == "black" & TouchdownDF$Choice == "touchdown"] <- 1
TouchdownDF$congruency[TouchdownDF$Stimuli == "white" & TouchdownDF$Choice == "touchdown"] <- 0
TouchdownDF$congruency[TouchdownDF$Stimuli == "black" & TouchdownDF$Choice == "no_touchdown"] <- 0
TouchdownDF$congruency <- as.factor(TouchdownDF$congruency)

#recode race based on reviewer comment

TouchdownDF$ethnicity_cat[TouchdownDF$Stimuli == "white" & TouchdownDF$Choice == "no_touchdown"] <- 1

#####

#effects code card probabilities
#####
CrimeDF$prob_eff <- as.factor(CrimeDF$prob)
contrasts(CrimeDF$prob_eff) <- contr.sum(8)
colnames(contrasts(CrimeDF$prob_eff)) = c(".5", ".55", ".56",".77", "83", "89", "92")
#####

#First order models: stereotype application vs. inhibition
#####
#effect of stimuli present on each trial
MetaM1<- glmer(acc~scale(Trial)*as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM1_coef <- summary(MetaM1) 

#RT based on stimuli present
CrimeDF$RT <- as.numeric(CrimeDF$RT)
MetaM2<- lmer(log(RT)~scale(Trial)+as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant), data = CrimeDF)
MetaM2_coef <- summary(MetaM2)
plot_model(MetaM2, type = "pred", terms = c("Stimuli"))

#RT for IMS
MetaM2<- lmer(log(RT)~scale(Trial)*scale(IMS)+ scale(EMS)+as.factor(study)+(scale(Trial)|Participant), data = CrimeDF)
MetaM2_coef <- summary(MetaM2)
tab_model(MetaM2)
plot_model(MetaM2, type = "pred", terms = c("Trial", "IMS"))
#are people less accurate on congruent vs. incongruent trials?
MetaM3<- glmer(acc~scale(Trial)*as.factor(congruency)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM3_coef <- summary(MetaM3) 

#are people slower for congruent vs. incongruent trials?
MetaM4<- glmer(log(RT)~scale(Trial)*as.factor(congruency)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM4_coef <- summary(MetaM4) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner?
MetaM5<- glmer(congruency~scale(Trial)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM5_coef <- summary(MetaM5) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner?
Test2<- glmer(congruency~scale(Trial)+scale(IMS)*scale(EMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
Test2_coef <- summary(Test2) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner differently over time?
MetaM6<- glmer(congruency~scale(Trial)*log(RT)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM6_coef <- summary(MetaM6) 

#does social dominance predict congruent responding?
MetaM7<- glmer(congruency~scale(Trial)*scale(SDO)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM7_coef <- summary(MetaM7) 

#do people rely more on stereotypes on less predictive trials?
MetaM8<- glmer(congruency~scale(Trial)+prob_eff+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM8_coef <- summary(MetaM8) 

#do people rely more on stereotypes on less predictive trials (abs deviation from .5)?
MetaM9<- glmer(congruency~scale(Trial)*scale(prob_abs)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
MetaM9_coef <- summary(MetaM9) 
#####


#second order models: cumulative effects over time. 
#####
#IMS predicting accuracy
MetaM10 <- glmer(acc~scale(Trial)*scale(IMS) + as.factor(study)+(scale(Trial)|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial",
                  control=glmerControl(optimizer="Nelder_Mead",
                                       optCtrl=list(maxfun=2e5))) #singularity is due to stimuli random effect, removing does not change estimates so keeping it in. 
MetaM10_coef<- summary(MetaM10) 
MetaM10_effects <- exp(fixef(MetaM10))
MetaM10_IMS.CI <- log(exp(confint(MetaM10,'scale(IMS)', level=0.95)))
MetaM10_IMS_Trial.CI <- log(exp(confint(MetaM10,'scale(Trial):scale(IMS)', level=0.95)))

#IMS predicting RT
MetaM11<- lmer(log(RT)~scale(Trial)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant), data = CrimeDF)
MetaM11_coef <- summary(MetaM11) 

#EMS predicting accuracy
MetaM12 <- glmer(acc~scale(Trial)*scale(EMS) + as.factor(study)+(Trial|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial")
MetaM12_coef<- summary(MetaM12) #singularity issue due to random effect of stimuli so dropping it
MetaM12_effects <- exp(fixef(MetaM12))
MetaM12_EMS.CI <- log(exp(confint(MetaM12,'scale(Trial):scale(EMS)', level=0.95)))

#IMS X EMS predicting accuracy
MetaM13 <- glmer(acc~scale(Trial)*scale(EMS)+scale(Trial)*scale(IMS)  + as.factor(study)+(Trial|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial")
summary(MetaM13)
tab_model()

#intergroup anxiety predicting accuracy
MetaM14<- glmer(acc~scale(Trial)*scale(intergroup_anx)+as.factor(study)+ (Trial|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial")
MetaM14_coef<- summary(MetaM14)

#EMS predicting accuracy in positive stereotype condition
MetaM15<- glmer(acc~scale(Trial)*scale(EMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = TouchdownDF, family = "binomial")
MetaM15_coef <- summary(MetaM15) #singularity issues with random effect of stimuli, removing converges but does not change estimate

#IMS predicting accuracy in positive stereotype condition
MetaM16<- glmer(acc~scale(Trial)*scale(IMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = TouchdownDF, family = "binomial")
MetaM16_coef <- summary(MetaM16) #singularity issues with random effect of stimuli, removing converges but does not change estimate
#####

#Figure 5. Visualize IMS effect. 
#####
#median split onyl for figure. Not good practice for analyses. 
CrimeDF$IMSDich[CrimeDF$IMS > median(CrimeDF$IMS, na.rm = T) ] <- "high"
CrimeDF$IMSDich[CrimeDF$IMS < median(CrimeDF$IMS, na.rm = T) ] <- "low"
plotColorIMS <- wes_palette(n= 2,"GrandBudapest1")
names(plotColorIMS) <- levels(CrimeDF$IMSDich)
meta1IMSPlot <- CrimeDF[!is.na(CrimeDF$IMSDich),] #remove missing for figure. 
IMS_plot <- ggplot(meta1IMSPlot, aes(Trial, acc, color = IMSDich)) + 
  geom_smooth(method = "loess") +scale_color_manual(values = plotColorIMS)+  
  theme(legend.position = "none", axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+theme(axis.ticks=element_blank())
#ggsave("IMS_plot.png", width = 4, height = 4, dpi = 700)
#####

print("FIRST ORDER MODELS")
print("no effect of stimuli on accuracy")
print(MetaM1_coef)
print("no effect of stimuli on reaction time")
print(MetaM2_coef)
print("no effect of congruency on accuracy")
print(MetaM3_coef)
print("no effect of congruency on reaction time")
print(MetaM4_coef)
print("no effect of IMS on congruent responding")
print(MetaM5_coef)
print("no effect of IMS and reaction time on congruent responding")
print(MetaM6_coef)
print("no effect of SDO on congruent responding")
print(MetaM7_coef)
print("card pattern weights (categorical) does not predict more stereotype-congruent responding")
print(MetaM8_coef)
print("card pattern weights (abs/continous) does not predict more stereotype-congruent responding")
print(MetaM9_coef)

print("SECOND ORDER MODELS")
print("significant main effect of internal motivation to respond without prejudice")
print(MetaM10_coef)
print("OR for IMS effects")
print(MetaM10_effects)
print("CI for IMS main effect")
print(MetaM10_IMS.CI)
print("CI for IMS by Trial interaction")
print(MetaM10_IMS_Trial.CI)

print("significant time by ims interaction: igher IMS, slower over time")
print(MetaM11_coef)

print("Marginal effect of EMS on accuracy")
print(MetaM12_coef)
print("OR for EMS effect")
print(MetaM12_effects)
print("CI for EMS by Trial interaction")
print(MetaM12_IMS_Trial.CI)

print("no effect of intergroup anxiety")
print(MetaM13_coef)

print("both IMS and EMS are not significant predictors of accuracy in positively-valenced condition")
print("IMS")
print(MetaM14_coef)
print("EMS")
print(MetaM15_coef)
