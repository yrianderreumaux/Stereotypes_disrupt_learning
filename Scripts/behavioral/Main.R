###This is the main file to reproduce the statistical analyses 
###for the manuscript entitled stereotypes disrupt probabilistic category learning

###########Remember to set the current directory as working directory before running this code##########

########Read in packages used for the analyses using groundhog#########
source('scripts/behavioral/Packages.R')

#######Script for Exclusion Criterion
source('scripts/behavioral/ParticipantExclusion.R')

#######Script for Individual Differences
source('scripts/behavioral/IndividualDifferences.R')

######Analyses for Study 1
source('scripts/behavioral/Study1.Analyses.R')

#######Analyses for Study 2
source('scripts/behavioral/Study2.Analyses.R')

#######Analyses for Study 3
source('scripts/behavioral/Study3.Analyses.R')

#######Analyses for Aggregated Analysis. Warning, this script includes multiple models and CI estimates that take some time to run. 
source('scripts/behavioral/Aggregated.Analysis.R')

#######Script for Power Analyses
source('scripts/powersim.R')




