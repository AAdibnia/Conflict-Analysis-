rm(list = ls())

#setwd("~/.../")

library(stargazer)
library(dummies)
library("openxlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(plyr)
library(readr)
library(dplyr)


templedata <- read_excel("Temple_Desecration.xlsx")

templedata$Statecode_district <- as.character(paste(templedata$STATECODE, templedata$District, sep = "_"))

templedata<-cbind(templedata,dummy(templedata$Statecode_district,sep = "_"))
templedata<-cbind(templedata,dummy(templedata$Statecode_district,sep = "_"))

agg_district <- aggregate(templedata[,9:ncol(templedata)], by=list(templedata$Statecode_district), FUN=sum)
agg_district$number_of_desecrations <- rowSums(agg_district[,2:ncol(agg_district)])
agg_district <- plyr::rename(agg_district, c("Group.1"="Statecode_district"))
agg_district$dummies_desecration <- 1

agg_district <- agg_district[,-c(2:(ncol(agg_district)-2))]

conflict_data <- read_excel("All_conflict_corrected.xlsx")

conflict_data$Statecode_district <- as.character(paste(conflict_data$STATECODE, conflict_data$DISTRICT, sep = "_"))

conflict_data$DISTRICT[is.na(conflict_data$DISTRICT)] <- conflict_data$TOWN_CITY[is.na(conflict_data$DISTRICT)]
conflict_data$DISTRICT[is.na(conflict_data$DISTRICT)] <- conflict_data$VILLAGE[is.na(conflict_data$DISTRICT)]
conflict_data$dummy_conflict <- 1


All_data <- merge(x = conflict_data, y = agg_district, by = "Statecode_district", all.x = TRUE)



population_statistics_91 <- read_excel("population_statistics_91.xlsx")
population_statistics_91 = select(population_statistics_91,-c(STATECODE))

All_data<-merge(x = All_data, y = population_statistics_91, by = "Statecode_district", all.x = FALSE)

All_data$dummy_conflict[is.na(All_data$dummy_conflict)] <- 0
All_data$KILLED[is.na(All_data$KILLED)] <- 0
All_data$INJURED[is.na(All_data$INJURED)] <- 0
All_data$ARRESTS[is.na(All_data$ARRESTS)] <- 0
All_data$dummies_desecration[is.na(All_data$dummies_desecration)] <- 0
All_data$number_of_desecrations[is.na(All_data$number_of_desecrations)] <-0
All_data$DURATION_I[is.na(All_data$DURATION_I)] <- 0
All_data$YEAR[is.na(All_data$YEAR)] <- -10

All_data$unweighted_conf <- All_data$INJURED+All_data$KILLED+All_data$ARRESTS
All_data$weighted_conf <- .5*All_data$INJURED+All_data$KILLED+.25*All_data$ARRESTS

################################## 1985 - 2000

#All_data = All_data[All_data$YEAR>1985,]


##################################  Ratio - ols

Results_dummies<- glm(dummy_conflict ~ dummies_desecration+factor(YEAR)+factor( STATECODE )+muslims_ratio+hindus_ratio, data=All_data) 

Results_weight<- glm(weighted_conf ~ dummies_desecration+factor(YEAR)+factor( STATECODE )+muslims_ratio+hindus_ratio, data=All_data) 

Results_killed<- glm(KILLED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )+muslims_ratio+hindus_ratio, data=All_data)

Results_injured<- glm(INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )+muslims_ratio+hindus_ratio, data=All_data) 

Results_arrests<- glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE )+muslims_ratio+hindus_ratio, data=All_data) 

Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed, 
                                 Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts","Weighted", "Killed",
                                                  "Injured", "Arrests")))


##################################  Ratio _ poisson

Results_dummies<- glm(dummy_conflict ~ factor( STATECODE )+factor(YEAR) + dummies_desecration
                      + muslims_ratio + hindus_ratio,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_weight <- glm(weighted_conf ~  dummies_desecration+factor(YEAR)+factor( STATECODE )
                        + muslims_ratio + hindus_ratio,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_killed <- glm(KILLED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )           
                        + muslims_ratio + hindus_ratio,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_injured <- glm(INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )
                         + muslims_ratio + hindus_ratio,
                       family = poisson(link = "log"),
                       data=All_data) 

Results_arrests <- glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                         + muslims_ratio + hindus_ratio,
                       family = poisson(link = "log"),
                       data=All_data) 

Results_duration <- glm(DURATION_I ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                       + muslims_ratio + hindus_ratio,
                       family = poisson(link = "log"),
                       data=All_data) 


Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed, Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts","Weighted", "Killed", "Injured", "Arrests")))

################################## Fraction - ols

Results_dummies<- glm(dummy_conflict ~ factor( STATECODE )+factor(YEAR) + dummies_desecration
                      + muslims_ratio + hindus_ratio + fraction,
                      data=All_data) 

Results_weight<- glm(weighted_conf ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                     + muslims_ratio + hindus_ratio  + fraction,
                     data=All_data) 

Results_killed<- glm(KILLED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )  
                       + muslims_ratio + hindus_ratio    + fraction,
                     data=All_data)

Results_injured<- glm(INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE ) 
                        + muslims_ratio + hindus_ratio       + fraction,
                      data=All_data) 

Results_arrests<- glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE ) 
                        + muslims_ratio + hindus_ratio      + fraction,
                      data=All_data) 

Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed,
                                 Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", 
                                 omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts","Weighted", "Killed", "Injured", "Arrests"),
                                 type="latex", out="logit.tex"))


##################################  Fraction _ poisson

Results_dummies <- glm( dummy_conflict ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                         + muslims_ratio + hindus_ratio + fraction,
                        family = poisson(link = "log"),
                        data=All_data) 

Results_weight <- glm( weighted_conf ~  dummies_desecration+factor(YEAR)+factor( STATECODE )      
                         + muslims_ratio + hindus_ratio + fraction,
                       family = poisson(link = "log"),
                       data=All_data)  

Results_killed <- glm( KILLED ~   dummies_desecration+factor(YEAR)+factor( STATECODE )     
                         + muslims_ratio + hindus_ratio + fraction,
                       family = poisson(link = "log"),
                       data=All_data) 

Results_injured <- glm( INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )     
                          + muslims_ratio + hindus_ratio + fraction,
                        family = poisson(link = "log"),
                        data=All_data)  

Results_arrests = glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE )     
                        + muslims_ratio + hindus_ratio + fraction,
                      family = poisson(link = "log"),
                      data=All_data) 


Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed, Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts","Weighted", "Killed", "Injured", "Arrests")))


##################################  Polarization - ols

Results_dummies<- glm(dummy_conflict ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                      + muslims_ratio + hindus_ratio   + polar,
                      data=All_data) 

Results_weight<- glm(weighted_conf ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                     + muslims_ratio + hindus_ratio   + polar,
                     data=All_data) 

Results_killed<- glm(KILLED ~  dummies_desecration+factor(YEAR)+factor( STATECODE ) 
                       + muslims_ratio + hindus_ratio   + polar,
                     data=All_data)

Results_injured<- glm(INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE ) 
                        + muslims_ratio + hindus_ratio   + polar,
                      data=All_data) 

Results_arrests<- glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE ) 
                        + muslims_ratio + hindus_ratio   + polar,
                      data=All_data) 

Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed, Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts","Weighted", "Killed", "Injured", "Arrests")))


##################################  Polarization _ poisson

Results_dummies<- glm(dummy_conflict ~ factor( STATECODE )+factor(YEAR) + dummies_desecration
                        + muslims_ratio + hindus_ratio   + polar,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_weight <- glm(weighted_conf ~  dummies_desecration+factor(YEAR)+factor( STATECODE )
                        + muslims_ratio + hindus_ratio   + polar,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_killed <- glm(KILLED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )        
                        + muslims_ratio + hindus_ratio   + polar,
                      family = poisson(link = "log"),
                      data=All_data) 

Results_injured <- glm(INJURED ~  dummies_desecration+factor(YEAR)+factor( STATECODE )
                         + muslims_ratio + hindus_ratio   + polar,
                       family = poisson(link = "log"),
                       data=All_data) 

Results_arrests <- glm(ARRESTS ~  dummies_desecration+factor(YEAR)+factor( STATECODE )     
                         + muslims_ratio + hindus_ratio   + polar,
                       family = poisson(link = "log"),
                       data=All_data) 


Resultss <- data.frame(stargazer(Results_dummies , Results_weight, Results_killed, Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Number of Conflicts")))
                                 

################################## OLS _ duration

Results_duration_ratio_OLS <- glm(DURATION_I ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                      + muslims_ratio + hindus_ratio,
                      data=All_data) 


Results_duration_fraction_OLS <- glm(DURATION_I ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                      + muslims_ratio + hindus_ratio+ fraction,
                      data=All_data) 


Results_duration_polar_OLS <- glm(DURATION_I ~ dummies_desecration+factor(YEAR)+factor( STATECODE )
                      + muslims_ratio + hindus_ratio + polar,
                      data=All_data) 


##################################  Poisson _ duration

Results_duration_ratio <- glm(DURATION_I ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                        family = poisson(link = "log"),
                        data=All_data) 


Results_duration_fraction <- glm(DURATION_I ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                              + muslims_ratio + hindus_ratio  + fraction,
                              family = poisson(link = "log"),
                              data=All_data) 

Results_duration_polar <- glm(DURATION_I ~  dummies_desecration+factor(YEAR)+factor( STATECODE )       
                              + muslims_ratio + hindus_ratio   + polar,
                              family = poisson(link = "log"),
                              data=All_data) 


Resultss <- data.frame(stargazer(Results_duration_ratio , Results_duration_fraction, Results_duration_polar,
                                 Results_duration_ratio_OLS , Results_duration_fraction_OLS,
                                 Results_duration_polar_OLS,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Duration of Conflicts")))
