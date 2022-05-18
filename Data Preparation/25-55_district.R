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

templedata <- templedata[25:55,]

write.xlsx(templedata, file = "Temple_des_final.xlsx")


################################# CONFLICT DATA

conflict_1996_2000 <- read_excel("~/Desktop/PhD/Second year/Second year paper/HM conflict 1996-2000.xlsx")

conflict_1950_1995 <- read_csv("~/Desktop/PhD/Second year/Second year paper/V-W_dataset_1950-1995_Oct_2004_version_no_notes.csv")

total_conflict <- rbind(conflict_1996_2000, conflict_1950_1995 )
attach(total_conflict)


write.xlsx(total_conflict, file = "All_conflict.xlsx")

################################# MERGING THE CONFLCIT AND DESCRATION DATA BASED ON DISTRICT

rm(list = ls())

templedata <- read_excel("~/Desktop/PhD/Second year/Second year paper/Temple_des_final.xlsx")
attach(templedata)

templedata<-cbind(templedata,dummy(District,sep = "_"))

agg_district <- aggregate(templedata[,7:30], by=list(District), FUN=sum)
agg_district$number_of_conflict <- rowSums(agg_district[,2:25])
agg_district <-  rename(agg_district, c("Group.1"="DISTRICT"))
agg_district$dummies_district <- 1

agg_district <- agg_district[,-c(2:25)]

conflictdata <- read_excel("All_conflict.xlsx")
attach(conflictdata)

conflictdata$DISTRICT[is.na(conflictdata$DISTRICT)] <- conflictdata$TOWN_CITY[is.na(conflictdata$DISTRICT)]
conflictdata$DISTRICT[is.na(conflictdata$DISTRICT)] <- conflictdata$VILLAGE[is.na(conflictdata$DISTRICT)]

MERGED<-merge(x = conflictdata, y = agg_district, by = "DISTRICT", all.x = TRUE)

MERGED$KILLED[is.na(MERGED$KILLED)] <- 0
MERGED$INJURED[is.na(MERGED$INJURED)] <- 0
MERGED$ARRESTS[is.na(MERGED$ARRESTS)] <- 0
MERGED$dummies_district[is.na(MERGED$dummies_district)] <- 0
MERGED$number_of_conflict[is.na(MERGED$number_of_conflict)] <-0

MERGED$unweighted_conf <- MERGED$INJURED+MERGED$KILLED+MERGED$ARRESTS

MERGED$weighted_conf <- .5*MERGED$INJURED+MERGED$KILLED+.25*MERGED$ARRESTS

Results_weight<- lm(weighted_conf ~ dummies_district, data=MERGED) 

Results_unweight<- lm(unweighted_conf ~ dummies_district, data=MERGED) 

Results_killed<- lm(KILLED ~ dummies_district, data=MERGED)

Results_injured<- lm(INJURED ~ dummies_district, data=MERGED) 

Results_arrests<- lm(ARRESTS ~ dummies_district, data=MERGED) 

stargazer(Results_unweight,Results_weight, Results_killed, Results_injured, Results_arrests,
          title="Regression Results",  notes.align ="c" , omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Unweighted","Weighted", "Killed", "Injured", "Arrests"),
          covariate.labels=c("Dummy for Districts"))





