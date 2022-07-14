#Load Data study 1
#####
id <- "14pDoVVaf9X1wp0gXn7x2kAFPRtRJO7uC" # google file ID
Study1IndDiff <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
colnames(Study1IndDiff)[5] <- "participant_strategy"
#descriptives of individual differences, demographics and number of samples
Study1.gender.prop<- prop.table(table(Study1IndDiff$Gender))
Study1.ethnicity.prop <- prop.table(table(Study1IndDiff$Ethnicity))
Study1.income <- hist(Study1IndDiff$Age)
#####

#Load Study 2
#####
id <- "1hVhWUzRQ--hhPxDhj9rIsvo5-R2wpVHk" # google file ID
Study2IndDiff <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
Study2IndDiff <- Study2IndDiff[!duplicated(Study2IndDiff$Participant),] #remove duplicate IDs
Study2.gender.prop <- prop.table(table(Study2IndDiff$gender))
Study2.ethnicity.prop <- prop.table(table(Study2IndDiff$ethnicity))
Study2.age <- hist(table(Study2IndDiff$age))
#individual differences
Study2.IMS <- hist(table(Study2IndDiff$IMS))
Study2.EMS <- hist(table(Study2IndDiff$EMS))  
Study2.SDO <- hist(Study2IndDiff$SDO) 
Study2.EX <- hist(Study2IndDiff$EX) 
Study2.intergroup_anx <- hist(Study2IndDiff$intergroup_anx) 
#psychometrics of scales
Study2.IMS.Omega <- Study2IndDiff$IMS.Omega
Study2.EMS.Omega<- Study2IndDiff$EMS.Omega
Study2.SDO.Omega<- Study2IndDiff$SDO.Omega
Study2.intergroup_anx.Omega<- Study2IndDiff$intergroup_anx.Omega

#Load study 3
#####
id <- "1RTDjNCXteFWVrpLVy8pOz5TNSIhHR_KG" # google drive file ID
Study3IndDiff <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),  header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Study3IndDiff <- Study3IndDiff[!duplicated(Study3IndDiff$Participant),] #remove duplicate IDs
Study3.gender.prop <- prop.table(table(Study3IndDiff$gender))
Study3.ethnicity.prop <- prop.table(table(Study3IndDiff$ethnicity))
Study3.age <- hist(table(Study3IndDiff$age))
#individual differences
Study3.IMS <- hist(Study3IndDiff$IMS)
Study3.EMS <- hist(Study3IndDiff$EMS)  
Study3.SDO <- hist(Study3IndDiff$SDO) 
Study3.EX <- hist(Study3IndDiff$EX) 
Study3.intergroup_anx <- hist(Study3IndDiff$intergroup_anx) 
#####

#####Print the results in the order they appear in the manuscript
print('Descriptives for Study 1')
print(Study1.gender.prop)
print(Study1.ethnicity.prop)
plot(Study1.income)

print('Descriptives for Study 2')
print(Study2.gender.prop)
print(Study2.ethnicity.prop)
plot(Study2.age)
plot(Study2.IMS)
plot(Study2.EMS)
plot(Study2.SDO)
plot(Study2.EX)
print('IMS Omega')
print(unique(Study2.IMS.Omega))
print('EMS Omega')
print(unique(Study2.EMS.Omega))
print('SDO Omega')
print(unique(Study2.SDO.Omega))

print('Descriptives for Study 3')
print(Study3.gender.prop)
print(Study3.ethnicity.prop)
plot(Study3.age)
plot(Study3.IMS)
plot(Study3.EMS)
plot(Study3.SDO)
plot(Study3.EX)

