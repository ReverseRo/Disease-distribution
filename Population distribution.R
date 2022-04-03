require(tableone)
require(stringr)

#### load the data file ####
file<- str_c(getwd(), "/data2018.RData")
load(file)
data$vision<-factor(data$vision)

#### calculate the prevalence ####
# by age
tableone<-CreateTableOne(vars="vision",strata=c("agegroup"),data=data,addOverall = T)
table1<-print(tableone,contDigits=1, showAllLevels=T, explain=F, varLabels=T, noSpaces=T)
# ba sex
tableone<-CreateTableOne(vars="vision",strata=c("male"),data=data,addOverall = T)
table1<-print(tableone,contDigits=1, showAllLevels=T, explain=F, varLabels=T, noSpaces=T)
# by residence
tableone<-CreateTableOne(vars="vision",strata=c("urban"),data=data,addOverall = T)
table1<-print(tableone,contDigits=1, showAllLevels=T, explain=F, varLabels=T, noSpaces=T)