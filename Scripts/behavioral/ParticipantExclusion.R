#Study 1
#####
id <- "1pAzA-_qaOdso1NIWYsz89uHXXMuBHHur" # google file ID
Study1exclus <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#exclusion by condition
table(Study1exclus$STIMULI.1[which(Study1exclus$avg_acc<.52)])
##show # of participants under 52% accurate
study1Outliers <- Study1exclus$P.[which(Study1exclus$avg_acc<=.52)] #11 participants under 52%
study1Outliers<- length(unique(study1Outliers))
#uncomment to show effect at 50% accuracy threshold
# Study1exclus <- Study1exclus %>% group_by(P.) %>% mutate(Trial = seq_along(P.))#first add trial variable
# fifty_exclusion<- glmer(Optimal.Response~scale(Trial)*as.factor(STIMULI.1)+ (scale(Trial)|P.), data = Study1exclus, family = "binomial")
# summary(fifty_exclusion) 
#remove outliters
Study1exclus_unique <- Study1exclus[!duplicated(Study1exclus$P.),]
Study1ExclusByCond <- table(Study1exclus_unique$STIMULI.1[which(Study1exclus_unique$avg_acc<.52)])
#Total Sample for study 1 after exclusion = 100
#####

#Study2
#####
id <- "1MK2RM6zavPqwUtrnmCrtRZBpf9oH0zCg" # google drive file ID
Study2exclus <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#show # of participants under 52% accurate
study2Outliers <- Study2exclus$Participant[which(Study2exclus$avg_acc<=.52)] #11 participants under 52%
study2Outliers<- length(unique(study2Outliers))
#by condition
Study2Exclus_unique <- Study2exclus[!duplicated(Study2exclus$Participant),]
Study2ExclusByCond <- table(Study2Exclus_unique$Condition[which(Study2Exclus_unique$avg_acc<.52)])
#Total Sample for study 2 after exclusion = 373
#####

#Study3
#####
id <- "1RTDjNCXteFWVrpLVy8pOz5TNSIhHR_KG" # google drive file ID
Study3exclus <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#calculate average accuracy for each participant
averga_acc <- Study3exclus %>%
  group_by(Participant)%>%
  dplyr::summarise(avg_acc=mean(acc, na.rm = T))
Study3exclus <- merge(Study3exclus, averga_acc, by = c("Participant"), all.x = F) #merge back with og df

#show # of participants under 52% accurate
study3Outliers <- Study3exclus$Participant[which(Study3exclus$avg_acc<=.52)] #15 participants under 52%
study3Outliers<- length(unique(study3Outliers))
#exclusion by condition
Study3Exclus_unique <- Study3exclus[!duplicated(Study3exclus$Participant),]
Study3ExclusByCond <- table(Study3Exclus_unique$Condition[which(Study3Exclus_unique$avg_acc<.52)])
#Total Sample for study 3 after exclusion = 205
#####


print("Participants Excluded for under 52% accuracy across Studies 1-3")
print("Study 1 Outliers")
print(study1Outliers)

print("Study 2 Outliers")
print(study2Outliers)

print("Study 3 Outliers")
print(study3Outliers)

print("Number of participants exlcuded based on condition across Studies 1-3")
print("Study 1 by condition")
print(Study1ExclusByCond)

print("Study 2 by condition")
print(Study2ExclusByCond)

print("Study 3 by condition")
print(Study3ExclusByCond)
