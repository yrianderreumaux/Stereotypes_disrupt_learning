#warning! this script will take quite a long time to run due to the amount of models that need to be estimated

#Load data from public google drive
#####
id <- "1u60kB6co6zekQz7HKo1qh2sHauzhVbis" # google file ID
CrimeDF <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
#add factor for study covariate
CrimeDF$study[CrimeDF$Condition=="steal"] <- "two"
CrimeDF$study[CrimeDF$Condition=="neg"] <- "three"

#load positively-valenced data to test individual differences
id <- "1yOdUdZXuzAAaP6n7bva92NC1XdtH8QtI" # google file ID
TouchdownDF <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
TouchdownDF$Participant <- as.factor(TouchdownDF$Participant) #make participant factor
#####

#create categorical card weight and congruency variables
#####
#crime condition
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


#Athletic condition
TouchdownDF$prob[TouchdownDF$Pattern == 1] <- .8947
TouchdownDF$prob[TouchdownDF$Pattern == 2] <- .7778
TouchdownDF$prob[TouchdownDF$Pattern == 3] <- .9231
TouchdownDF$prob[TouchdownDF$Pattern == 5] <- .8333
TouchdownDF$prob[TouchdownDF$Pattern == 6] <- .8947
TouchdownDF$prob[TouchdownDF$Pattern == 9] <- .55

TouchdownDF$prob[TouchdownDF$Pattern == 4 ] <- 1-.2222
TouchdownDF$prob[TouchdownDF$Pattern == 7] <- 1-.1053
TouchdownDF$prob[TouchdownDF$Pattern == 8 ] <- 1-.1667
TouchdownDF$prob[TouchdownDF$Pattern == 10 ] <- 1-.07
TouchdownDF$prob[TouchdownDF$Pattern == 11 ] <- 1-.44
TouchdownDF$prob[TouchdownDF$Pattern == 12] <- 1-.1053

TouchdownDF$prob[TouchdownDF$Pattern == 13] <- .5
TouchdownDF$prob[TouchdownDF$Pattern == 14] <- .5

#also code as absolute deviation from .5
TouchdownDF$prob_abs[TouchdownDF$Pattern == 1] <- abs(.8947-.5)
TouchdownDF$prob_abs[TouchdownDF$Pattern == 2] <- abs(.7778-.5)
TouchdownDF$prob_abs[TouchdownDF$Pattern == 3] <- abs(.9231-.5)
TouchdownDF$prob_abs[TouchdownDF$Pattern == 5] <- abs(.8333-.5)
TouchdownDF$prob_abs[TouchdownDF$Pattern == 6] <- abs(.8947-.5)
TouchdownDF$prob_abs[TouchdownDF$Pattern == 9] <- abs(.55-.5)

TouchdownDF$prob_abs[TouchdownDF$Pattern == 4 ] <- abs((1-.2222)-(.5))
TouchdownDF$prob_abs[TouchdownDF$Pattern == 7] <- abs((1-.1053)-(.5))
TouchdownDF$prob_abs[TouchdownDF$Pattern == 8 ] <- abs((1-.1667)-(.5))
TouchdownDF$prob_abs[TouchdownDF$Pattern == 10 ] <- abs((1-.07)-(.5))
TouchdownDF$prob_abs[TouchdownDF$Pattern == 11 ] <- abs((1-.44)-(.5))
TouchdownDF$prob_abs[TouchdownDF$Pattern == 12] <- abs((1-.1053)-(.5))

TouchdownDF$prob_abs[TouchdownDF$Pattern == 13] <- .5 - .5
TouchdownDF$prob_abs[TouchdownDF$Pattern == 14] <- .5 - .5

#prototypicality 
CrimeDF$prototyp[CrimeDF$Face_Shown==1] <- 3.897435897
CrimeDF$prototyp[CrimeDF$Face_Shown==2] <- 3.70212766
CrimeDF$prototyp[CrimeDF$Face_Shown==3] <- 3.914893617
CrimeDF$prototyp[CrimeDF$Face_Shown==4] <- 4.189189189
CrimeDF$prototyp[CrimeDF$Face_Shown==5] <- 4.14285714
CrimeDF$prototyp[CrimeDF$Face_Shown==6] <- 4.25
CrimeDF$prototyp[CrimeDF$Face_Shown==7] <- 3.955555556
CrimeDF$prototyp[CrimeDF$Face_Shown==8] <- 4.038461538

#create stereotypical congruency variables
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
#####

#effects code card probabilities
#####
CrimeDF$prob_eff <- as.factor(CrimeDF$prob)
contrasts(CrimeDF$prob_eff) <- contr.sum(8)
colnames(contrasts(CrimeDF$prob_eff)) = c(".5", ".55", ".56",".77", "83", "89", "92")

TouchdownDF$prob_eff <- as.factor(TouchdownDF$prob)
contrasts(TouchdownDF$prob_eff) <- contr.sum(8)
colnames(contrasts(TouchdownDF$prob_eff)) = c(".5", ".55", ".56",".77", "83", "89", "92")
#####

#make reaction time numeric (RT)
#####
CrimeDF$RT <- as.numeric(CrimeDF$RT)
TouchdownDF$RT <- as.numeric(TouchdownDF$RT)
#####

                                         ###########   First order models in the Crime Face: stereotype application vs. inhibition  #############

#####
#effect of stimuli present on each trial
first_order_M1<- glmer(acc~scale(Trial)*as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
first_order_M1_coef <- summary(first_order_M1) 
first_order_M1_CI <- log(exp(confint(first_order_M1,'Stimuli', level=0.95)))

#are people less accurate on congruent vs. incongruent trials?
first_order_M2<- glmer(acc~scale(Trial)*as.factor(congruency)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
first_order_M2_coef <- summary(first_order_M2) 

#are people slower for congruent vs. incongruent trials?
first_order_M3<- lmer(log(RT)~scale(Trial)*as.factor(congruency)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF)
first_order_M3 <- summary(first_order_M3) 

#do people rely more on stereotypes on less predictive trials?
first_order_M4<- glmer(congruency~scale(Trial)+prob_eff+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
first_order_M4 <- summary(first_order_M4) 

#do people rely more on stereotypes on less predictive trials (abs deviation from .5)?
first_order_M5<- glmer(congruency~scale(Trial)*scale(prob_abs)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
first_order_M5 <- summary(first_order_M5) 

#does social dominance predict congruent responding?
first_order_M6<- glmer(congruency~scale(Trial)*scale(SDO)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant)+ (scale(SDO)|Participant), data = CrimeDF, family = "binomial")
first_order_M6_coef <- summary(first_order_M6) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner?
first_order_M7<- glmer(congruency~scale(Trial)*scale(IMS)+ as.factor(study)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = CrimeDF, family = "binomial")
first_order_M7_coef <- summary(first_order_M7) 
#####

                                ###########   First order models in the Athletic condition: stereotype application vs. inhibition  #############

#####
#note that singular fit warnings are due to low variance within subjects. However, removing this random effect does not change interpretations and therefore is included. 
#effect of stimuli present on each trial
first_order_M1_pos<- glmer(acc~scale(Trial)*as.factor(Stimuli)+ (scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF, family = "binomial")
first_order_M1_pos_coef <- summary(first_order_M1_pos) 

#are people less accurate on congruent vs. incongruent trials?
first_order_M2_pos<- glmer(acc~scale(Trial)*as.factor(congruency)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF, family = "binomial")
ffirst_order_M2_pos_coef <- summary(first_order_M2_pos) 

#are people slower for congruent vs. incongruent trials?
first_order_M3_pos<- lmer(log(RT)~scale(Trial)*as.factor(congruency)+ (scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF)
first_order_M3_pos_coef <- summary(first_order_M3_pos) 

#do people rely more on stereotypes on less predictive trials?
first_order_M4_pos<- glmer(congruency~scale(Trial)+prob_eff+ (scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF, family = "binomial")
first_order_M4_pos_coef <- summary(first_order_M4_pos) 

#do people rely more on stereotypes on less predictive trials (abs deviation from .5)?
first_order_M5_pos<- glmer(congruency~scale(Trial)*scale(prob_abs)+ (scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF, family = "binomial")
first_order_M5_pos_coef <- summary(first_order_M5_pos) 

#does social dominance predict congruent responding?
first_order_M6_pos<- glmer(congruency~scale(Trial)*scale(SDO)+ (scale(Trial)|Participant)+(as.factor(Stimuli)|Participant)+ (scale(SDO)|Participant), data = TouchdownDF, family = "binomial")
first_order_M6_pos_coef <- summary(first_order_M6_pos) 

#does IMS moderate whether people are more likely to respond in a stereotype congruent manner?
first_order_M7_pos<- glmer(congruency~scale(Trial)*scale(IMS)+(scale(Trial)|Participant)+(as.factor(Stimuli)|Participant), data = TouchdownDF, family = "binomial")
first_order_M7_pos_coef <- summary(first_order_M7_pos) 
#####

                                 ###########   Second order models in Crime Face Condition: cumulative effects over time  #############

 
#####
#IMS predicting accuracy
second_order_M1 <- glmer(acc~scale(Trial)*scale(IMS) + as.factor(study)+(scale(Trial)|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial",
                  control=glmerControl(optimizer="Nelder_Mead",
                                       optCtrl=list(maxfun=2e5))) #singularity is due to stimuli random effect, removing does not change estimates so keeping it in. 
second_order_M1_coef<- summary(second_order_M1) 
second_order_M1_effects <- exp(fixef(second_order_M1))
second_order_M1_IMS.CI <- log(exp(confint(second_order_M1,'scale(IMS)', level=0.95)))
second_order_M1_Trial_IMS.CI <- log(exp(confint(second_order_M1,'scale(Trial):scale(IMS)', level=0.95)))

#EMS predicting accuracy
second_order_M2 <- glmer(acc~scale(Trial)*scale(EMS) + as.factor(study)+(Trial|Participant)+ (1|Face_Shown), data = CrimeDF, family = "binomial")
second_order_M2_coef<- summary(second_order_M2) #singularity issue due to random effect of stimuli so dropping it
second_order_M2_effects <- exp(fixef(second_order_M2))
second_order_M2.CI <- log(exp(confint(second_order_M2,'scale(Trial):scale(EMS)', level=0.95)))

#Three-way interaction with IMS, EMS and trial
second_order_M3<- glmer(acc~scale(Trial)*scale(IMS)*scale(EMS)+ as.factor(study)+(scale(Trial)|Participant), data = CrimeDF, family = "binomial")
second_order_M3_coef <- summary(second_order_M3) 
second_order_M3_effects <- exp(fixef(second_order_M3))
second_order_M3.CI <- log(exp(confint(second_order_M3,'scale(Trial):scale(IMS)', level=0.95)))

#IMS and RT association 
second_order_M4<- lmer(log(RT)~scale(Trial)*scale(IMS)+ scale(EMS)+as.factor(study)+(scale(Trial)|Participant), data = CrimeDF)
second_order_M4_coef <- summary(second_order_M4)
second_order_M4_effects <- exp(fixef(second_order_M4))
second_order_M4.CI <- log(exp(confint(second_order_M4,'scale(Trial):scale(IMS)', level=0.95)))

#RT based on stimuli present
second_order_M5<- lmer(log(RT)~scale(Trial)*as.factor(Stimuli)+ as.factor(study)+(scale(Trial)|Participant), data = CrimeDF)
second_order_M5_coef <- summary(second_order_M5)
second_order_M5.CI <- log(exp(confint(second_order_M5,'as.factor(Stimuli)white', level=0.95)))
plot_model(second_order_M5, type = "pred", terms = c("Stimuli"))
#####

                             ###########   Second order models in Athletic Condition: cumulative effects over time  #############


#####
#IMS predicting accuracy in positive stereotype condition 
second_order_M1_pos<- glmer(acc~scale(Trial)*scale(IMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = TouchdownDF, family = "binomial")
second_order_M1_pos_coef <- summary(second_order_M1_pos) #singularity issues with random effect of stimuli, removing converges but does not change estimate

#EMS predicting accuracy in positive stereotype condition
second_order_M2_pos<- glmer(acc~scale(Trial)*scale(EMS)+ (scale(Trial)|Participant)+ (1|Face_Shown), data = TouchdownDF, family = "binomial")
second_order_M2_pos_coef <- summary(second_order_M2_pos) #singularity issues with random effect of stimuli, removing converges but does not change estimate

#Three-way interaction with IMS, EMS and trial
second_order_M3_pos<- glmer(acc~scale(Trial)*scale(IMS)*scale(EMS)+(scale(Trial)|Participant), data = TouchdownDF, family = "binomial")
second_order_M3_pos_coef <- summary(second_order_M3_pos) 

#RT based on stimuli present
second_order_M4_pos<- lmer(log(RT)~scale(Trial)*scale(IMS)+ scale(EMS)+(scale(Trial)|Participant), data = TouchdownDF)
second_order_M4_pos_coef <- summary(second_order_M4_pos)

#RT based on stimuli present
second_order_M5_pos<- lmer(log(RT)~scale(Trial)*as.factor(Stimuli)+(scale(Trial)|Participant), data = TouchdownDF)
second_order_M5_pos_coef <- summary(second_order_M5_pos)


#####

#Figure 5. 
#####
#median split for figure only.
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

print("FIRST ORDER MODELS in Crime Face condition")

print("no effect of stimuli on accuracy")
print(first_order_M1_coef)

print("no effect of congruency on accuracy")
print(first_order_M2_coef)

print("no effect of congruency on reaction time")
print(first_order_M3_coef)

print("card pattern weights (categorical) does not predict more stereotype-congruent responding")
print(first_order_M4_coef)

print("card pattern weights (abs/continous) does not predict more stereotype-congruent responding")
print(first_order_M5_coef)

print("no effect of SDO on congruent responding")
print(first_order_M6_coef)

print("no effect of IMS/EMS on congruent responding")
print(first_order_M7_coef)


print("FIRST ORDER MODELS in Athletic condition")

print("no effect of stimuli on accuracy")
print(first_order_M1_pos_coef)

print("no effect of congruency on accuracy")
print(first_order_M2_pos_coef)

print("no effect of congruency on reaction time")
print(first_order_M3_pos_coef)

print("card pattern weights (categorical) does not predict more stereotype-congruent responding")
print(first_order_M4_pos_coef)

print("card pattern weights (abs/continous) does not predict more stereotype-congruent responding")
print(first_order_M5_pos_coef)

print("no effect of SDO on congruent responding")
print(first_order_M6_pos_coef)

print("no effect of IMS/EMS on congruent responding")
print(first_order_M7_pos_coef)


print("SECOND ORDER MODELS in Crime Face condition")

print("M1: IMS association with accuracy")

print("M1: significant main effect of internal motivation to respond without prejudice")
print(second_order_M1_coef)
print("OR for IMS effects")
print(second_order_M1_effects)
print("CI for IMS main effect")
print(second_order_M1_IMS.CI)
print("CI for IMS by Trial interaction")
print(second_order_M1_Trial_IMS.CI)

print("M2: EMS association with accuracy")

print("Marginal main effect of EMS")
print(second_order_M2_coef)
print("OR for EMS effects")
print(second_order_M2_effects)
print("CI for EMS main effect")
print(second_order_M2.CI)

print("M3: testing three-way interaction with IMS, EMS and trial")

print("IMS still significant after controlling for EMS")
print(second_order_M3_coef)
print("OR for IMS with EMS ")
print(second_order_M3_effects)
print("CI for IMS by Trial with EMS")
print(second_order_M3.CI)

print("M4: IMS associated with slower RT")

print("Higher IMS associated with slower RT over time")
print(second_order_M4_coef)
print("OR for effect")
print(second_order_M4_effects)
print("Ci for effect")
print(second_order_M4.CI)

print("M5: RT based on face present")

print("Black faces associated with longer RT over time")
print(second_order_M5_coef)
print("OR for effect")
print(second_order_M5_effects)
print("Ci for effect")
print(second_order_M5.CI)

print("SECOND ORDER MODELS in Athletic Condition")

print("M1: IMS not associated accuracy")
print(second_order_M1_pos_coef)

print("M2: EMS not associated with accuracy")
print(second_order_M2_pos_coef)

print("M3: testing three-way interaction with IMS, EMS and trial")
print(second_order_M3_pos_coef)

print("M4: No IMS by RT association")
print(second_order_M4_pos_coef)

print("M5: no association with RT based on face present")
print(second_order_M5_pos_coef)

