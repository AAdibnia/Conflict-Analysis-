### In the census data, religion91.dta' we have the state code but we also need the abberevatio of each state. This code adds the state's abberevatio to each row, based on the state id code.


religion91 <- read_dta("religion91.dta")

religion91$STATECODE = "NaN"

religion91$STATECODE[religion91$stateid==2]= "AP" #

religion91$STATECODE[religion91$stateid==4]= "AS" #

religion91$STATECODE[religion91$stateid==5]= "BI" #

religion91$STATECODE[religion91$stateid==31]= "DE" #

religion91$STATECODE[religion91$stateid==7]= "GU" #

religion91$STATECODE[religion91$stateid==8]= "HA" #

religion91$STATECODE[religion91$stateid==11]= "KA" #

religion91$STATECODE[religion91$stateid==12]= "KE" #

religion91$STATECODE[religion91$stateid==13]= "MP" #

religion91$STATECODE[religion91$stateid==14]= "MA" #

religion91$STATECODE[religion91$stateid==15]= "MN" #

religion91$STATECODE[religion91$stateid==19]= "OR" #

religion91$STATECODE[religion91$stateid==20]= "PU" #

religion91$STATECODE[religion91$stateid==21]= "RA" #

religion91$STATECODE[religion91$stateid==23]= "TN" #

religion91$STATECODE[religion91$stateid==24]= "TR" #

religion91$STATECODE[religion91$stateid==25]= "UP" #

religion91$STATECODE[religion91$stateid==26]= "WB" #

religion91 = religion91[religion91$STATECODE!="NaN",]

religion91 = religion91[religion91$disttype=="Total",]



religion91 = religion91[,-c(12:31)]

religion91 = religion91[,-c(9:10)]

religion91 = religion91[,-c(6:7)]

religion91 = religion91[,-c(1:2)]

religion91 = religion91[,-c(2)]


##### district + state_code

religion91$Statecode_district = "NaN"

religion91$Statecode_district =  as.character(paste(religion91$STATECODE, religion91$districtrel , sep = "_")) #



## Population ratio

religion91$muslims_ratio = religion91$muslims/religion91$totpersons

religion91$hindus_ratio = religion91$hindus/religion91$totpersons


## Fractionalization

religion91$fraction = religion91$muslims_ratio*(1-religion91$muslims_ratio) + religion91$hindus_ratio*(1-religion91$hindus_ratio)


## Polarization

religion91$polar = (religion91$muslims_ratio^2)*(1-religion91$muslims_ratio) + (religion91$hindus_ratio^2)*(1-religion91$hindus_ratio)

write.xlsx(religion91, file = "population_statistics_91.xlsx")









