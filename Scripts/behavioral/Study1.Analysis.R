#Load data from public google drive
id <- "17W8lGp9yJeSCv8bxRLSuy1hlLXQtPv_n" # google file ID
Study1Data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Study1Data$Participant <- as.factor(Study1Data$Participant) #make participant factor

##Dummy code condition for accuracy mixed model (weather face as reference)
#####
Study1Data$Condition_dum <- as.factor(Study1Data$Condition)#made factor
contrasts(Study1Data$Condition_dum) <- contr.treatment(2)#dummy code
colnames(contrasts(Study1Data$Condition_dum)) = c("Faces")#specify reference group
#####

#Run mixed model for learning model with random effect of trial within subjects and random intercept for stimuli
#####
Study1.model<- glmer(Acc~scale(Trial)*Condition_dum+ (scale(Trial)|Participant) +(1 | Stimuli), data = Study1Data, family = "binomial")
#random effect for face giving rise to singularity issue. Removing it does not change effects
Study1.coef <- summary(Study1.model)
Study1.effects <- exp(fixef(Study1.model))
Study1.effects.CI_trial <- confint(Study1.model, level=0.95)
Study1.effects.CI <- confint(Study1.model, 'Condition_dumFaces', level=0.95)
#####

#Figure 2 in paper
#####
plotcurve1 <- ggplot(Study1Data, aes(Trial, Acc, fill = as.factor(Condition))) + 
  geom_smooth(method = "loess", color ="grey50")+
  scale_y_continuous(name = "Accuracy")+scale_fill_manual(breaks = c("Weather", "Faces"),
                                                          values = c("#C6CDF7", "#D8A499"))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_cartesian(ylim = c(.6, .9))+theme(axis.ticks=element_blank())
#ggsave("plotcurve1", device='jpeg', dpi=700)
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FOR STUDY 1')
print('logistic mixed model for learning rates')
print(Study1.coef)
print(Study1.effects)
print(Study1.effects.CI_trial)
print(Study1.effects.CI)

