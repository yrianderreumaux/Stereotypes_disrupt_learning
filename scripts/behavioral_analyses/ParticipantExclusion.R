#Study 1
#####
Study1exclus <- read.csv("data/raw data/study1Raw.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Study1exclus$Optimal.Response <- as.integer(as.character(Study1exclus$Optimal.Response))
averga_acc <- Study1exclus %>%
  group_by(P.)%>%
  dplyr::summarise(avg_acc=mean(Optimal.Response, na.rm = T))
Study1exclus <- merge(Study1exclus, averga_acc, by = c("P."), all.x = F)

#show # of participants under 52% accurate
study1Outliers <- Study1exclus$P.[which(Study1exclus$avg_acc<=.52)] #11 participants under 52%
study1Outliers<- length(unique(study1Outliers))
table(Study1exclus$STIMULI.1[which(Study1exclus$avg_acc<.52)])
#Total Sample for study 1 after exclusion = 100
#####

#Study2
#####
Study2exclus <- read.csv("data/raw data/study2Raw.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
averga_acc <- Study2exclus %>%
  group_by(Participant)%>%
  dplyr::summarise(avg_acc=mean(acc, na.rm = T))
Study2exclus <- merge(Study2exclus, averga_acc, by = c("P."), all.x = F)
#show # of participants under 52% accurate
study2Outliers <- Study2exclus$Participant[which(Study2exclus$avg_acc<=.52)] #11 participants under 52%
study2Outliers<- length(unique(study2Outliers))
#by condition
Study2exclus_unique <- Study2exclus[!duplicated(Study2exclus$Participant),]
table(Study2exclus_unique$Condition[which(Study2exclus_unique$avg_acc<.52)])
#Total Sample for study 1 after exclusion = 373
#####

#Study3
#####
Study3exclusv<- read.csv("data/raw data/study3Raw.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
averga_acc <- Study3exclusv %>%
  group_by(Participant)%>%
  dplyr::summarise(avg_acc=mean(acc, na.rm = T))
Study3exclusv <- merge(Study3exclusv, averga_acc, by = c("Participant"), all.x = F)

#show # of participants under 52% accurate
study3Outliers <- Study3exclusv$Participant[which(Study3exclusv$avg_acc<=.52)] #15 participants under 52%
study3Outliers<- length(unique(study3Outliers))
#Total Sample for study 1 after exclusion = 205
#####


print("Participants Excluded for under 52% accuracy across Studies 1-3")
print("Study 1 Outliers")
print(study1Outliers)

print("Study 2 Outliers")
print(study2Outliers)

print("Study 3 Outliers")
print(study3Outliers)

