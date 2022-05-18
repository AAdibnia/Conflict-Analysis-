rm(list = ls())

setwd("~/Desktop/PhD/Second year/Second year paper/")

install.packages("xlsx")
install.packages("stargazer")
install.packages('dummies')

library(stargazer)
library(dummies)
library("openxlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(plyr)
library(readr)


################################# CLEAR TEMPLE DESECRATION DATA

rm(list = ls())


templedata <- read_excel("Temple_Desecration.xlsx")
attach(templedata)

templedata <- templedata[1:24,]

write.xlsx(templedata, file = "Temple_des_final_24.xlsx")


#################################  CONFLICT DATA

conflict_1996_2000 <- read_excel("~/Desktop/PhD/Second year/Second year paper/HM conflict 1996-2000.xlsx")

conflict_1950_1995 <- read_csv("~/Desktop/PhD/Second year/Second year paper/V-W_dataset_1950-1995_Oct_2004_version_no_notes.csv")

total_conflict <- rbind(conflict_1996_2000, conflict_1950_1995 )
attach(total_conflict)

write.xlsx(total_conflict, file = "All_conflict.xlsx")


################################# Cleaning the census data

rm(list = ls())

templedata <- read_excel("Temple_des_final_24.xlsx")
attach(templedata)


templedata<-cbind(templedata,dummy(District,sep = "_"))

agg_district <- aggregate(templedata[,7:63], by=list(District), FUN=sum)
agg_district$number_of_conflict <- rowSums(agg_district[,2:58])
agg_district <- plyr::rename(agg_district, c("Group.1"="DISTRICT"))
agg_district$dummies_district <- 1
agg_district <- agg_district[,-c(2:58)]

india_districts_census_2011 <- read_csv("india-districts-census-2011.csv")

india_districts_census_2011 <-india_districts_census_2011[,-c(28:118)]
india_districts_census_2011 <-india_districts_census_2011[,-c(5:25)]
india_districts_census_2011 <-india_districts_census_2011[,-c(1:2)]

india_districts_census_2011<-plyr::rename(india_districts_census_2011, c("District name"="DISTRICT"))

MERGED<-merge(x = india_districts_census_2011, y = agg_district, by = "DISTRICT", all.x = TRUE)

MERGED$dummies_district[is.na(MERGED$dummies_district)] <-0
MERGED$number_of_conflict[is.na(MERGED$number_of_conflict)] <-0


#################################
Compares <- data.frame("DISTRICT"= c(MERGED$DISTRICT[MERGED$dummies_district==1],rep("Na",2 ))
                       ,"Des District" = rep(1,57))

MERGED_to_find_unmatched<-merge(x = agg_district, y = Compares, by = "DISTRICT", all.x = TRUE)

MERGED_to_find_unmatched<-data.frame("DISTRICT"=c(MERGED_to_find_unmatched$DISTRICT[is.na(MERGED_to_find_unmatched$Des.District)]))


################################# MERGING THE CONFLCIT AND DESCRATION DATA BASED ON DISTRICT

rm(list = ls())

templedata <- read_excel("~/Desktop/PhD/Second year/Second year paper/Temple_des_final_24.xlsx")
attach(templedata)


templedata<-cbind(templedata,dummy(District,sep = "_"))

agg_district <- aggregate(templedata[,7:27], by=list(District), FUN=sum)
agg_district$number_of_conflict <- rowSums(agg_district[,2:20])
agg_district <-  rename(agg_district, c("Group.1"="DISTRICT"))
agg_district$dummies_district <- 1

agg_district <- agg_district[,-c(2:22)]


conflictdata <- read_excel("All_conflict.xlsx")
attach(conflictdata)

conflictdata$DISTRICT[is.na(conflictdata$DISTRICT)] <- conflictdata$TOWN_CITY[is.na(conflictdata$DISTRICT)]
conflictdata$DISTRICT[is.na(conflictdata$DISTRICT)] <- conflictdata$VILLAGE[is.na(conflictdata$DISTRICT)]


All_data<-merge(x = conflictdata, y = agg_district, by = "DISTRICT", all.x = TRUE)

All_data$KILLED[is.na(All_data$KILLED)] <- 0
All_data$INJURED[is.na(All_data$INJURED)] <- 0
All_data$ARRESTS[is.na(All_data$ARRESTS)] <- 0
All_data$dummies_district[is.na(All_data$dummies_district)] <- 0
All_data$number_of_conflict[is.na(All_data$number_of_conflict)] <-0


india_districts_census_2011 <- read_csv("india-districts-census-2011.csv")

india_districts_census_2011 <-india_districts_census_2011[,-c(28:118)]
india_districts_census_2011 <-india_districts_census_2011[,-c(5:25)]
india_districts_census_2011 <-india_districts_census_2011[,-c(1:2)]

india_districts_census_2011<-plyr::rename(india_districts_census_2011, c("District name"="DISTRICT"))

All_data<-merge(x = All_data, y = india_districts_census_2011, by = "DISTRICT", all.x = TRUE)


All_data$Population[is.na(All_data$Population)] <-1
All_data$Muslims[is.na(All_data$Muslims)] <-.5
All_data$Hindus[is.na(All_data$Hindus)] <-.5

All_data$Muslim_frac <- All_data$Muslims/All_data$Population

All_data$Hindu_frac <- All_data$Hindus/All_data$Population

All_data$unweighted_conf <- All_data$INJURED+All_data$KILLED+All_data$ARRESTS

All_data$weighted_conf <- .5*All_data$INJURED+All_data$KILLED+.25*All_data$ARRESTS

All_data <-cbind(All_data,dummy(All_data$YEAR,sep = "_"))

Results_weight<- lm(weighted_conf ~ factor( STATE )+factor(YEAR)+dummies_district+Muslim_frac+Hindu_frac, data=All_data) 

Results_unweight<- lm(unweighted_conf ~  factor( STATE )+factor(YEAR)+dummies_district+Muslim_frac+Hindu_frac, data=All_data) 

Results_killed<- lm(KILLED ~  factor( STATE )+factor(YEAR)+dummies_district+Muslim_frac+Hindu_frac, data=All_data)

Results_injured<- lm(INJURED ~  factor( STATE )+factor(YEAR)+dummies_district+Muslim_frac+Hindu_frac, data=All_data) 

Results_arrests<- lm(ARRESTS ~  factor( STATE )+factor(YEAR)+dummies_district+Muslim_frac+Hindu_frac, data=All_data) 


Resultss <- data.frame(stargazer(Results_unweight, Results_weight, Results_killed, Results_injured, Results_arrests,
                                 title="Regression Results",  notes.align ="c", omit.stat=c("LL","ser","f"),
                                 dep.var.labels=c("Unweighted","Weighted", "Killed", "Injured", "Arrests")))

write.table(Resultss[,1], "mydata.txt", sep=",")

















