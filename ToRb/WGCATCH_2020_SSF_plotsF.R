library(readxl)
library(data.table)
library(ggplot2)
options(scipen=999)

work.directory<-"C:/Sébastien/Dossiers_courant/_WGCATCH_20151105/2020/Questionnaires/Questionnaires_WGCATCH_2020_meeting/Analysis_2020"
setwd(work.directory)
getwd()

#Load the tables
table2<-read_xlsx("SSF_Overwiev_tables_2018_2020_withGreeceEstimated_VF.xlsx",sheet="Sheet1")
table3<-read_xlsx("SSF_Overwiev_tables_2018_2020_withGreeceEstimated_VF.xlsx",sheet="Sheet2")
head(table2)
head(table3)

#Change column names
t2.colnames<-c("Country","Country_code","Supra.region","Area","ICES.region","Country_area","Year","Year_max","Vessel.length.range",
               "Registered.vessels","Active.vessels","Scientific_estimates","LSF")
t3.colnames<-c("Country","Country_code","Supra.region","Area","ICES.region","Country_area","Year","Year_max","Vessel.length.range",
               "Trips.range","nb.Vessels","Scientific_estimates","LSF")
colnames(table2)<-t2.colnames
colnames(table3)<-t3.colnames

sum(table2$Registered.vessels,na.rm=TRUE)
sum(table2$Active.vessels,na.rm=TRUE)

table2<-data.table(table2)
#table(table2$Status)
#test<-table2[Status=='ESTIMATED' | Status=='C']
#head(test)
#table(test$Status)
test <- table2
########################################################
####### Filter the most recent submitted year ##########  
########################################################
dim(test)
table(test$Year)
sum(test$Registered.vessels,na.rm=TRUE)
test$NB <- 1
test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Year,Year_max)]
test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Country_area,Year,Year_max)]
test<-test[test[, .I[Year == max(Year)], by=Country]$V1]
dim(test)
table(test$Year)
sum(test$Registered.vessels,na.rm=TRUE)
test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Year,Year_max)]
test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Country_area,Year,Year_max)]

test<-test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE)), 
           .(Country,Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates,LSF)]
head(test)
dim(test)
test$NB <- 1
test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Country_area)]
table(test$Country)
table(test$Country_code)
table(test$Country_area)
dim(unique(test[,c("Country_area")]))
table(test$Area)
table(test$Vessel.length.range)
table2F<-test

table3<-data.table(table3)
#table(table3$Status)
#test<-table3[Status=='ESTIMATED' | Status=='C']
#head(test)
test<-table3
########################################################
####### Filter the most recent submitted year ##########  
########################################################
dim(test)
table(test$Year)
sum(test$nb.Vessels,na.rm=TRUE)
sum(table2$Active.vessels,na.rm=TRUE)
test$NB <- 1
test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Year,Year_max)]
test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Country_area,Year,Year_max)]
test<-test[test[, .I[Year == max(Year)], by=Country]$V1]
dim(test)
sum(test$nb.Vessels,na.rm=TRUE)
sum(table2F$Active.vessels,na.rm=TRUE)
test$NB <- 1
test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Year,Year_max)]
test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE),NB=sum(NB,na.rm=TRUE)), 
     .(Country,Country_code,Country_area,Year,Year_max)]
#table(test$Status)
test<-test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE)), 
           .(Country,Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates,LSF,Trips.range)]
table(test$Country)
table(test$Country_code)
table(test$Country_area)
dim(unique(test[,c("Country_area")]))
dim(unique(table2F[,c("Country_area")]))
table(test$Area)
table(test$Vessel.length.range)
table3F<-test

#Calculate the number of active vessels from table2 and insert into table3
test<-table2F
test<-test[, .(Country_area, Vessel.length.range,Active.vessels)]
head(test)
dim(test)
head(table3F)
dim(table3F)
table3F <- merge(table3F,test,by=c("Country_area","Vessel.length.range"),all.x=TRUE)
dim(table3F)
head(table3F)

table3F[, perc:=round(nb.Vessels*100/Active.vessels,2)]
colnames(table3F)
table3F<-table3F[,.(Country,Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates,LSF,Trips.range,nb.Vessels,perc)]
head(table3F)

### ajout des classe de trips => inactive vessels and active vessels dans la table 3F
table2F[,Inactive.vessels:=Registered.vessels-Active.vessels]
inactive<-table2F[,.(Country,Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates,LSF,Inactive.vessels)]
inactive[,Trips.range:="6: inactive"]
setnames(inactive,old="Inactive.vessels", new="nb.Vessels")
test<-inactive

active<-table2F[,.(Country, Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates, LSF,Active.vessels)]
active[,Trips.range:="7: active"]
setnames(active,old="Active.vessels", new="nb.Vessels")
test<-rbind(test,active)
head(test)

register<-table2F[, .(Country, Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates,LSF,Registered.vessels)]
head(register)
test <- merge(test,register,by=c("Country","Country_code","Country_area","Area","Vessel.length.range","Scientific_estimates","LSF"),all.x=TRUE)
test[, perc:=round(nb.Vessels*100/Registered.vessels,2)]
head(test)
test<-test[,.(Country, Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates, LSF,Trips.range,nb.Vessels,perc)]
head(test)

dim(test)
table(test$Trips.range)
dim(table3F)
table(table3F$Trips.range)
table3F <- rbind(table3F,test)
dim(table3F)
table(table3F$Trips.range)

register<-table2F[,.(Country,Country_code,Country_area,Area,Vessel.length.range,Scientific_estimates, LSF,Registered.vessels)]
register[,Trips.range:="8: registered"]
setnames(register,old="Registered.vessels", new="nb.Vessels")
#test<-unique(test)
register$perc=register$nb.Vessels
register<-register[,c("Country","Country_code","Country_area","Area","Vessel.length.range","Scientific_estimates","LSF","Trips.range","nb.Vessels","perc")]
#test[,Registered.vessels:=NULL]

dim(register)
table(register$Trips.range)
dim(table3F)
table(table3F$Trips.range)
table3F <- rbind(table3F,register)
dim(table3F)
table(table3F$Trips.range)

table(table3F$Vessel.length.range)
table3F$Vessel.length.rangeF <- "CL1"
table3F[table3F$Vessel.length.range %in% c("2: 6-8"),]$Vessel.length.rangeF <- "CL2"
table3F[table3F$Vessel.length.range %in% c("3: 8-10"),]$Vessel.length.rangeF <- "CL3"
table3F[table3F$Vessel.length.range %in% c("4: 10-12"),]$Vessel.length.rangeF <- "CL4"
table3F[table3F$Vessel.length.range %in% c("5: 12-15"),]$Vessel.length.rangeF <- "CL1"
table3F[table3F$Vessel.length.range %in% c("6: 15-18"),]$Vessel.length.rangeF <- "CL2"
table3F[table3F$Vessel.length.range %in% c("7: 18-24"),]$Vessel.length.rangeF <- "CL3"
table3F[table3F$Vessel.length.range %in% c("8: 24-XX"),]$Vessel.length.rangeF <- "CL4"

table3F[,Id:=paste0(Vessel.length.rangeF," (",Trips.range,")")]
head(table3F)

table4<-table3F[,.(Country_area,Scientific_estimates,LSF,Id,nb.Vessels)]
head(table4)
table5<-dcast(table4, Country_area+Scientific_estimates+LSF~Id, value.var = "nb.Vessels", fun.aggregate = sum)
head(table5)
#table5[is.na(table5)]<-0
getwd()
write.csv(table5,"No_of_vessels.csv",row.names = FALSE)

table4<-table3F[,.(Country_area,Scientific_estimates,LSF,Id,perc)]
head(table4)
table5<-dcast(table4, Country_area+Scientific_estimates+LSF~Id, value.var = "perc", fun.aggregate = sum)
head(table5)
#table5[is.na(table5)]<-0

table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (1: 1-9)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (2: 10-49)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (3: 50-99)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (4: 100-149)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (5: >=150)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (6: inactive)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (7: active)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (1: 1-9)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (2: 10-49)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (3: 50-99)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (4: 100-149)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (5: >=150)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (6: inactive)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (7: active)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (1: 1-9)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (2: 10-49)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (3: 50-99)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (4: 100-149)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (5: >=150)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (6: inactive)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (7: active)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (1: 1-9)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (2: 10-49)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (3: 50-99)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (4: 100-149)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (5: >=150)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (6: inactive)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (7: active)`<-NA
head(table5)

table5[table5$`CL1 (7: active)` == 0,]$`CL1 (1: 1-9)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (2: 10-49)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (3: 50-99)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (4: 100-149)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (5: >=150)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (1: 1-9)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (2: 10-49)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (3: 50-99)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (4: 100-149)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (5: >=150)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (1: 1-9)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (2: 10-49)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (3: 50-99)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (4: 100-149)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (5: >=150)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (1: 1-9)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (2: 10-49)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (3: 50-99)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (4: 100-149)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (5: >=150)`<-NA
head(table5)

colnames(table5)
dim(table5)
colnames(table5) <- c("Country_area","Scientific_estimates","LSF","CL1_1-9","CL1_10-49","CL1_50-99","CL1_100-149","CL1_p150","CL1_INA","CL1_ACT","CL1_nbvessels",
                      "CL2_1-9","CL2_10-49","CL2_50-99","CL2_100-149","CL2_p150","CL2_INA","CL2_ACT","CL2_nbvessels",
                      "CL3_1-9","CL3_10-49","CL3_50-99","CL3_100-149","CL3_p150","CL3_INA","CL3_ACT","CL3_nbvessels",
                      "CL4_1-9","CL4_10-49","CL4_50-99","CL4_100-149","CL4_p150","CL4_INA","CL4_ACT","CL4_nbvessels")
colnames(table5)
dim(table5)
getwd()
write.csv(table5,"Perc_of_vessels.csv",row.names = FALSE)

#Calculate the number of active vessels from table2 and insert into table3
test<-table2
test<-test[, .(Country_area, Vessel.length.range,Active.vessels)]
head(test)
dim(test)
head(table3)
dim(table3)
table3 <- merge(table3,test,by=c("Country_area","Vessel.length.range"),all.x=TRUE)
dim(table3)
head(table3)

table3[, perc:=round(nb.Vessels*100/Active.vessels,2)]
colnames(table3)
table3<-table3[,.(Year,Country,Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates,LSF,Trips.range,nb.Vessels,perc)]
head(table3)

table2[,Inactive.vessels:=Registered.vessels-Active.vessels]
inactive<-table2[,.(Year,Country,Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates,LSF,Inactive.vessels)]
inactive[,Trips.range:="6: inactive"]
setnames(inactive,old="Inactive.vessels", new="nb.Vessels")
test<-inactive

active<-table2[,.(Year,Country, Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates, LSF,Active.vessels)]
active[,Trips.range:="7: active"]
setnames(active,old="Active.vessels", new="nb.Vessels")
test<-rbind(test,active)
head(test)

register<-table2[, .(Year,Country, Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates,LSF,Registered.vessels)]
head(register)
test <- merge(test,register,by=c("Year","Country","Country_code","Country_area","Area","Vessel.length.range","Year_max","Scientific_estimates","LSF"),all.x=TRUE)
test[, perc:=round(nb.Vessels*100/Registered.vessels,2)]
head(test)
test<-test[,.(Year,Country, Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates, LSF,Trips.range,nb.Vessels,perc)]
head(test)

dim(test)
table(test$Trips.range)
dim(table3)
table(table3$Trips.range)
table3 <- rbind(table3,test)
dim(table3)
table(table3$Trips.range)

register<-table2[,.(Year,Country,Country_code,Country_area,Area,Vessel.length.range,Year_max,Scientific_estimates, LSF,Registered.vessels)]
register[,Trips.range:="8: registered"]
setnames(register,old="Registered.vessels", new="nb.Vessels")
#test<-unique(test)
register$perc=register$nb.Vessels
register<-register[,c("Year","Country","Country_code","Country_area","Area","Vessel.length.range","Year_max","Scientific_estimates","LSF","Trips.range","nb.Vessels","perc")]
#test[,Registered.vessels:=NULL]

dim(register)
table(register$Trips.range)
dim(table3)
table(table3$Trips.range)
table3 <- rbind(table3,register)
dim(table3)
table(table3$Trips.range)

table(table3$Vessel.length.range)
table3$Vessel.length.rangeF <- "CL1"
table3[table3$Vessel.length.range %in% c("2: 6-8"),]$Vessel.length.rangeF <- "CL2"
table3[table3$Vessel.length.range %in% c("3: 8-10"),]$Vessel.length.rangeF <- "CL3"
table3[table3$Vessel.length.range %in% c("4: 10-12"),]$Vessel.length.rangeF <- "CL4"
table3[table3$Vessel.length.range %in% c("5: 12-15"),]$Vessel.length.rangeF <- "CL1"
table3[table3$Vessel.length.range %in% c("6: 15-18"),]$Vessel.length.rangeF <- "CL2"
table3[table3$Vessel.length.range %in% c("7: 18-24"),]$Vessel.length.rangeF <- "CL3"
table3[table3$Vessel.length.range %in% c("8: 24-XX"),]$Vessel.length.rangeF <- "CL4"

table3[,Id:=paste0(Vessel.length.rangeF," (",Trips.range,")")]
head(table3)

table4<-table3[,.(Year,Country_area,Year_max,Scientific_estimates,LSF,Id,nb.Vessels)]
head(table4)
table5<-dcast(table4, Year+Country_area+Year_max+Scientific_estimates+LSF~Id, value.var = "nb.Vessels", fun.aggregate = sum)
head(table5)
#table5[is.na(table5)]<-0
getwd()
write.csv(table5,"No_of_vessels_complet.csv",row.names = FALSE)

table4<-table3[,.(Year,Country_area,Year_max,Scientific_estimates,LSF,Id,perc)]
head(table4)
table5<-dcast(table4, Year+Country_area+Year_max+Scientific_estimates+LSF~Id, value.var = "perc", fun.aggregate = sum)
head(table5)
#table5[is.na(table5)]<-0

table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (1: 1-9)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (2: 10-49)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (3: 50-99)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (4: 100-149)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (5: >=150)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (6: inactive)`<-NA
table5[table5$`CL1 (8: registered)` == 0,]$`CL1 (7: active)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (1: 1-9)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (2: 10-49)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (3: 50-99)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (4: 100-149)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (5: >=150)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (6: inactive)`<-NA
table5[table5$`CL2 (8: registered)` == 0,]$`CL2 (7: active)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (1: 1-9)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (2: 10-49)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (3: 50-99)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (4: 100-149)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (5: >=150)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (6: inactive)`<-NA
table5[table5$`CL3 (8: registered)` == 0,]$`CL3 (7: active)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (1: 1-9)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (2: 10-49)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (3: 50-99)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (4: 100-149)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (5: >=150)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (6: inactive)`<-NA
table5[table5$`CL4 (8: registered)` == 0,]$`CL4 (7: active)`<-NA
head(table5)

table5[table5$`CL1 (7: active)` == 0,]$`CL1 (1: 1-9)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (2: 10-49)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (3: 50-99)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (4: 100-149)`<-NA
table5[table5$`CL1 (7: active)` == 0,]$`CL1 (5: >=150)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (1: 1-9)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (2: 10-49)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (3: 50-99)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (4: 100-149)`<-NA
table5[table5$`CL2 (7: active)` == 0,]$`CL2 (5: >=150)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (1: 1-9)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (2: 10-49)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (3: 50-99)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (4: 100-149)`<-NA
table5[table5$`CL3 (7: active)` == 0,]$`CL3 (5: >=150)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (1: 1-9)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (2: 10-49)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (3: 50-99)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (4: 100-149)`<-NA
table5[table5$`CL4 (7: active)` == 0,]$`CL4 (5: >=150)`<-NA
head(table5)

colnames(table5)
dim(table5)
colnames(table5) <- c("Year","Country_area","Year_max","Scientific_estimates","LSF","CL1_1-9","CL1_10-49","CL1_50-99","CL1_100-149","CL1_p150","CL1_INA","CL1_ACT","CL1_nbvessels",
                      "CL2_1-9","CL2_10-49","CL2_50-99","CL2_100-149","CL2_p150","CL2_INA","CL2_ACT","CL2_nbvessels",
                      "CL3_1-9","CL3_10-49","CL3_50-99","CL3_100-149","CL3_p150","CL3_INA","CL3_ACT","CL3_nbvessels",
                      "CL4_1-9","CL4_10-49","CL4_50-99","CL4_100-149","CL4_p150","CL4_INA","CL4_ACT","CL4_nbvessels")
colnames(table5)
dim(table5)
getwd()
write.csv(table5,"Perc_of_vessels_complet.csv",row.names = FALSE)


### Fichier "Perc_of_vessels_complet.csv" table à utilisé dans les ACPs ####

# https://thinkr.fr/pdf/ggplot2-french-cheatsheet.pdf

### GRaphiques "Registered vs. active by country"

#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")

#table(table2F$Scientific_estimates)
#inter <- table2[table2F$Scientific_estimates %in% c("NON"),]
#table(inter$Scientific_estimates)
inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)

head(inter)
ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),
              Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_area)]
ot<-melt(ot, id=c("Country_area"), measure=c("Registered.vessels", "Declarative.data.vessels"))
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area",
                     "\n",
                     "(declarative data vessels vs registered)"), 
       x = "Country_area", 
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

p<-ggplot(ot[!(ot$Country_area %in% c("GRC_SSF_GSA22_2019","GRC_SSF_GSA22_2019sc","NOR_SSF_27_2017","PRT_SSF_27_2019")),],aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area",
               "\n",
               "(declarative data vessels vs registered)"), 
       x = "Country_area", 
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou2",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

sum(inter$Registered.vessels)
sum(inter$Active.vessels)
stats <- inter[,.(Registered.vessels=sum(Registered.vessels,na.rm=TRUE),Active.vessels=sum(Active.vessels,na.rm=TRUE)),
               by=.(Country_area)]
stats$Percentage <- (stats$Active.vessels/stats$Registered.vessels)*100
head(stats)

# p<-ggplot(ot[ot$Country_area %in% c("GRC_SSF_GSA22_2019","GRC_SSF_GSA22_2019sc","NOR_SSF_27_2017","PRT_SSF_27_2019"),],aes(fill=variable,y=value,x=Country_area))+
#   geom_bar(stat="identity",position="dodge")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   theme(plot.title = element_text(hjust = 0.5))+
#   labs(title = paste("Number of SSF vessels (<12m) by country*area",
#                      "\n",
#                      "(declarative data vessels vs registered)"), 
#        x = "Country_area", 
#        y = "Number of vessels",
#        fill="")
# fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou1",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

### Mémes graphiques pour les LSF - non mis dans le rapport mais construit au cas où ...
inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)

head(inter)
ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),
             Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area)]
ot<-melt(ot, id=c("Country_area"), measure=c("Registered.vessels", "Declarative.data.vessels"))
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of LSF vessels (>=12m) by country*area",
                     "\n",
                     "(declarative data vessels vs registered)"), 
       x = "Country_area", 
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_LSFVes_nb_by_cou",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

# p<-ggplot(ot[!(ot$Country_area %in% c("NOR_LSF_27_2017")),],aes(fill=variable,y=value,x=Country_area))+
#   geom_bar(stat="identity",position="dodge")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   theme(plot.title = element_text(hjust = 0.5))+
#   labs(title = paste("Number of LSF vessels (>=12m) by country*area",
#                      "\n",
#                      "(declarative data vessels vs registered)"), 
#        x = "Country_area", 
#        y = "Number of vessels",
#        fill="")
# fname <- paste0("Declarative_vs_Registered_LSFVes_nb_by_cou2",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

#Perc. registered vs. vessels with declarative data by country*area and vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_area,Vessel.length.range)]

test<-table2F[,.(nb.vessels=sum(Registered.vessels,na.rm = T)),
             by=.(Country_area)]
head(test)
dim(ot)
ot<-merge(ot,test,by=c("Country_area"))
dim(ot)
head(ot)
ot$perc.Registered.vessels <- (ot$Registered.vessels/ot$nb.vessels)*100
ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$nb.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Registered.vessels", "perc.Declarative.data.vessels"))
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_grid(.~variable)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF vessels (<12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country",
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_vs_Registered_SSFVes_perc_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]

test<-table2F[,.(nb.vessels=sum(Registered.vessels,na.rm = T)),
              by=.(Country_area)]
head(test)
dim(ot)
ot<-merge(ot,test,by=c("Country_area"))
dim(ot)
head(ot)
ot$perc.Registered.vessels <- (ot$Registered.vessels/ot$nb.vessels)*100
ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$nb.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Registered.vessels", "perc.Declarative.data.vessels"))
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_grid(.~variable)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of LSF vessels (>=12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country",
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_vs_Registered_LSFVes_perc_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)

table(inter[inter$Scientific_estimates %in% c("OUI"),]$Country_area)
table(inter$Scientific_estimates)
inter <- inter[inter$Country_area %in% c("FRA_SSF_GSA7_2019sc","FRA_SSF_GSA7_2019","FRA_SSF_GSA8_2019sc","FRA_SSF_GSA8_2019",
                                         "FRG_SSF_31_2019sc","FRG_SSF_31_2019","FRG_SSF_3141_2019sc","FRG_SSF_3141_2019",
                                         "FRM_SSF_31_2019sc","FRM_SSF_31_2019","FRM_SSF_51_2019sc","FRM_SSF_51_2019",
                                         "FRR_SSF_51_2019sc","FRR_SSF_51_2019","GRC_SSF_GSA22_2019sc","GRC_SSF_GSA22_2019"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]

test<-table2F[,.(nb.vessels=sum(Registered.vessels,na.rm = T)),
              by=.(Country_area)]
head(test)
dim(ot)
ot<-merge(ot,test,by=c("Country_area"))
dim(ot)
head(ot)
ot$perc.Registered.vessels <- (ot$Registered.vessels/ot$nb.vessels)*100
ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$nb.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Registered.vessels", "perc.Declarative.data.vessels"))
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_grid(.~variable)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of LSF vessels (>=12m) by country*area (incl. scientific estimates) and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country",
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_vs_Registered_SSFVes_scientific_perc_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

#Perc. declarative data vessels by country*area and vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_area,Vessel.length.range)]

ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$Registered.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
############### Remove NaN from dataset
ot<-na.omit(ot, cols = "value") 
#levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(y=value, x=Vessel.length.range)) +
  geom_bar(stat="identity", width = 0.99, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Country_area)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF vessels (<12m) with declarative data vs SSF registered vessels",
                     "\n",
                     "by country*area and vessel length range"),
       x = "Vessel length ranges",
       y = "% of SSF vessels (<12m) with declarative data vs SSF registered vessels")
fname <- paste0("Declarative_vs_Registered_SSFVes_perc_by_cou+loa2",".png")
ggsave(filename=fname, plot=p, width=15, height = 15)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

#Perc. declarative data vessels by country*area and vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]

ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$Registered.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
############### Remove NaN from dataset
ot<-na.omit(ot, cols = "value") 
#levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(y=value, x=Vessel.length.range)) +
  geom_bar(stat="identity", width = 0.99, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Country_area)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of LSF vessels (>=12m) with declarative data vs LSF registered vessels",
                     "\n",
                     "by country*area and vessel length range"),
       x = "Vessel length ranges",
       y = "% of LSF vessels (>=12m) with declarative data vs LSF registered vessels")
fname <- paste0("Declarative_vs_Registered_LSFVes_perc_by_cou+loa2",".png")
ggsave(filename=fname, plot=p, width=15, height = 15)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)

table(inter[inter$Scientific_estimates %in% c("OUI"),]$Country_area)
table(inter$Scientific_estimates)
inter <- inter[inter$Country_area %in% c("FRA_SSF_GSA7_2019sc","FRA_SSF_GSA7_2019","FRA_SSF_GSA8_2019sc","FRA_SSF_GSA8_2019",
                                         "FRG_SSF_31_2019sc","FRG_SSF_31_2019","FRG_SSF_3141_2019sc","FRG_SSF_3141_2019",
                                         "FRM_SSF_31_2019sc","FRM_SSF_31_2019","FRM_SSF_51_2019sc","FRM_SSF_51_2019",
                                         "FRR_SSF_51_2019sc","FRR_SSF_51_2019","GRC_SSF_GSA22_2019sc","GRC_SSF_GSA22_2019"),]
table(inter$Scientific_estimates)
head(inter)

#Perc. declarative data vessels by country*area and vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]

ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$Registered.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_area", "Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
############### Remove NaN from dataset
ot<-na.omit(ot, cols = "value") 
#levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(y=value, x=Vessel.length.range)) +
  geom_bar(stat="identity", width = 0.99, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Country_area)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF vessels (<12m) with declarative data or active regarding scientific estimates (sc) vs SSF registered vessels",
                     "\n",
                     "by country*area and vessel length range"),
       x = "Vessel length ranges",
       y = "% of SSF vessels (<12m) with declarative data or active regarding sceintific estimates (sc) vs SSF registered vessels")
fname <- paste0("Declarative_vs_Registered_SSFVes_scientific_perc_by_cou+loa2",".png")
ggsave(filename=fname, plot=p, width=15, height = 15)

# #Registered vs. active by area
# table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
# ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range)]
# ot<-melt(ot, id=c("Country_AREA", "Vessel.length.range"), measure=c("Registered.vessels", "Active.vessels"))
# 
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)
# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75, position="fill")+
#   scale_fill_brewer(palette="Set1")+
#   facet_grid(.~variable)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by country*area and vessel length range (registered vs. active)",
#        x = "Country",
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_perc_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

# #Registered vs. active by area en valeur/vessel length range
# table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
# ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range)]
# ot<-melt(ot, id=c("Country_AREA", "Vessel.length.range"), measure=c("Registered.vessels", "Active.vessels"))
# 
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)
# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75, position="stack")+
#   scale_fill_brewer(palette="Set1")+
#   facet_grid(.~variable)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by country*area and vessel length range (registered vs. active)",
#        x = "Country",
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_nb_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

#Registered vs. active by country by vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_area,Vessel.length.range)]
ot<-melt(ot, id=c("Country_area","Vessel.length.range"), measure=c("Registered.vessels", "Declarative.data.vessels"))
############### Remove NaN from dataset
#ot<-na.omit(ot, cols = "value") 
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country_area",
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

p<-ggplot(ot[!(ot$Country_area %in% c("GRC_SSF_GSA22_2019","GRC_SSF_GSA22_2019sc","NOR_SSF_27_2017","PRT_SSF_27_2019")),]
          ,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country_area",
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou2+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)


inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]
ot<-melt(ot, id=c("Country_area","Vessel.length.range"), measure=c("Registered.vessels", "Declarative.data.vessels"))
############### Remove NaN from dataset
#ot<-na.omit(ot, cols = "value") 
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of LSF vessels (>=12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country_area",
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_LSFVes_nb_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

#Registered vs. active by country by vessel length ranges
#table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_area,Vessel.length.range)]
ot$perc.Declarative.data.vessels <- ot$Declarative.data.vessels/ot$Registered.vessels
ot<-melt(ot, id=c("Country_area","Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
############### Remove NaN from dataset
ot<-na.omit(ot, cols = "value") 
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set2")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
               "\n",
               " by country*area and vessel length range"), 
       x = "Country_area", 
       y = "%r of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_perc_by_cou2+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

inter<-table2F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range)]
ot$perc.Declarative.data.vessels <- ot$Declarative.data.vessels/ot$Registered.vessels
ot<-melt(ot, id=c("Country_area","Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
############### Remove NaN from dataset
ot<-na.omit(ot, cols = "value") 
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_area))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set2")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of LSF declarative data vessels (>=12m)",
                     "\n",
                     " by country*area and vessel length range"), 
       x = "Country_area", 
       y = "%r of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_LSFVes_perc_by_cou2+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

############ Number of trips ranges - Graphs ###########################################
# #Percentage of number of vessels by country by area and vessel length range
# table3$Country_AREA <- paste(table3$Country, table3$Area,sep="_")
# ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active",
#            .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range, Trips.range)]
# levels<-sort(unique(ot$Trips.range),decreasing = T)
# p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75, position="fill")+
#   scale_fill_brewer(palette="Set1")+
#   facet_grid(.~Vessel.length.range)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of vessels by Country*area, vessel length range and number of trips range", 
#        x = "Country", 
#        y = "% of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_perc_by_cou+loa+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

# #Vessels by country and number of trips
# ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active"
#   ,.(nb.Vessels=sum(nb.Vessels,na.rm = T)),by=.(Country_AREA,Trips.range)]
# levels<-sort(unique(ot$Trips.range),decreasing = T)
# p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country*area and number of trips range", 
#        x = "Country*area", 
#        y = "Number of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_by_cou+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

# #Registered and active vessels by country and vessels length categories
# ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),
#               Active.vessels=sum(Active.vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range)]
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)
# 
# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=Registered.vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of registered SSF vessels by country*area and vessel length range", 
#        x = "Country*Area", 
#        y = "Number of vessels",
#        fill="Vessel length range")
# fname <- paste0("VesReg_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=Registered.vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75, position="fill")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of registered SSF vessels by country*area and vessel length range", 
#        x = "Country*Area", 
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("VesReg_perc_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=Active.vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of active SSF vessels by country*area and vessel length range", 
#        x = "Country*Area", 
#        y = "Number of vessels",
#        fill="Vessel length range")
# fname <- paste0("VesAct_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=Active.vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75,position="fill")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of active SSF vessels by country*area and vessel length range", 
#        x = "Country*Area", 
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("VesAct_perc_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)


# ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),
#               Active.vessels=sum(Active.vessels,na.rm = T)),
#            by=.(Area,Country,Vessel.length.range)]
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

# p<-ggplot(ot[Area=="27"]) +
#   geom_bar(aes(x = Country, y = Registered.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.8,alpha=0.5, stat = "identity") +
#   geom_bar(aes(x = Country, y = Active.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.25, stat = "identity") +
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country and vessel length range in area 27.\nRegistered(wide) vs. active(narrow)", 
#        x = "Country", 
#        y = "Number of vessels",
#        fill="Vessel length range") 
# fname <- paste0("Ves_by_cou+loa_area27",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)
# 
# p<-ggplot(ot[Area=="37"]) +
#   geom_bar(aes(x = Country, y = Registered.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.8,alpha=0.5, stat = "identity") +
#   geom_bar(aes(x = Country, y = Active.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.25, stat = "identity") +
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country and vessel length range in area 37. Registered\n(wide) vs. active(narrow)", 
#        x = "Country", 
#        y = "Number of vessels",
#        fill="Vessel length range") 
# fname <- paste0("Ves_by_cou+loa_area37",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)
# 
# p<-ggplot(ot[Area=="27"]) +
#   geom_bar(aes(x = Country, y = Registered.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.8,alpha=0.5, stat = "identity", position="fill") +
#   geom_bar(aes(x = Country, y = Active.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.25, stat = "identity", position="fill") +
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by country and vessel length range in area 27.\nRegistered(wide) vs. active(narrow)", 
#        x = "Country", 
#        y = "% of vessels",
#        fill="Vessel length range") 
# fname <- paste0("Ves_perc_by_cou+loa_area27",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)
# 
# p<-ggplot(ot[Area=="37"]) +
#   geom_bar(aes(x = Country, y = Registered.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.8,alpha=0.5, stat = "identity", position="fill") +
#   geom_bar(aes(x = Country, y = Active.vessels, fill=factor(Vessel.length.range,levels)), 
#            width = 0.25, stat = "identity", position="fill") +
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by country and vessel length range in area 37. \nRegistered (wide) vs. active(narrow)", 
#        x = "Country", 
#        y = "% of vessels",
#        fill="Vessel length range") 
# fname <- paste0("Ves_perc_by_cou+loa_area37",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)


#Vessels by country and number of trips
#table3$Country_AREA <- paste(table3$Country_code, table3$Area,sep="_")

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
           by=.(Country_area,Trips.range)]
levels<-sort(unique(ot$Trips.range),decreasing = T)


# p<-ggplot(ot[Trips.range!="0: inactive"], aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country*area and number of trips range", 
#        x = "Country", 
#        y = "Number of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_by_cou+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="fill")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
              "\n",
              "by country*area and number of trips range"), 
       x = "Country_area", 
       y = "% of vessels",
       fill="Number of trips range")
fname <- paste0("Declarative_SSFVes_perc_by_cou+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=8, height = 5)

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
          by=.(Country_area,Trips.range)]
levels<-sort(unique(ot$Trips.range),decreasing = T)


# p<-ggplot(ot[Trips.range!="0: inactive"], aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country*area and number of trips range", 
#        x = "Country", 
#        y = "Number of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_by_cou+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="fill")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of LSF declarative data vessels (>=12m)",
                     "\n",
                     "by country*area and number of trips range"), 
       x = "Country_area", 
       y = "% of vessels",
       fill="Number of trips range")
fname <- paste0("Declarative_LSFVes_perc_by_cou+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=8, height = 5)

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter[inter$Scientific_estimates %in% c("OUI"),]$Country_area)
table(inter$Scientific_estimates)
inter <- inter[inter$Country_area %in% c("FRA_SSF_GSA7_2019sc","FRA_SSF_GSA7_2019","FRA_SSF_GSA8_2019sc","FRA_SSF_GSA8_2019",
                                         "FRG_SSF_31_2019sc","FRG_SSF_31_2019","FRG_SSF_3141_2019sc","FRG_SSF_3141_2019",
                                         "FRM_SSF_31_2019sc","FRM_SSF_31_2019","FRM_SSF_51_2019sc","FRM_SSF_51_2019",
                                         "FRR_SSF_51_2019sc","FRR_SSF_51_2019","GRC_SSF_GSA22_2019sc","GRC_SSF_GSA22_2019"),]
table(inter$Scientific_estimates)
head(inter)


ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
          by=.(Country_area,Trips.range)]
levels<-sort(unique(ot$Trips.range),decreasing = T)


# p<-ggplot(ot[Trips.range!="0: inactive"], aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75)+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country*area and number of trips range", 
#        x = "Country", 
#        y = "Number of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_by_cou+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_area)) +
  geom_bar(stat="identity", width = 0.75, position="fill")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m, including scientific estimates)",
                     "\n",
                     "by country*area and number of trips range"), 
       x = "Country_area", 
       y = "% of vessels",
       fill="Number of trips range")
fname <- paste0("Declarative_SSFVes_scientific_perc_by_cou+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=8, height = 5)


# p<-ggplot(ot, aes(fill=Country_AREA, y=nb.Vessels, x=Trips.range)) +
#   geom_bar(stat="identity", width = 0.75, position="fill")+
#   #scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by number of trips range and country*area", 
#        x = "Number of trips range", 
#        y = "% of vessels",
#        fill="Number of trips range")
# fname <- paste0("Ves_perc_by_trips+cou_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=8, height = 5)

# Number of vessels by trip range, vessel length range for each country separately
# ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active",.(nb.Vessels=sum(nb.Vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range,Trips.range)]
# 
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

# p<-ggplot(ot[Trips.range!="0: inactive"])+
#   geom_bar(aes(x=Trips.range, y=nb.Vessels, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 315, hjust=0))+
#   facet_wrap( ~ Country_AREA, ncol=5)+
#   labs(title = "Number of SSF vessels by country, vessel length range and number of trips range in area 27", 
#        x = "Number of trips range", 
#        y = "Number of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_by_cou+loa+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=9, height=6)

# p<-ggplot(ot[Area=="37" & Trips.range!="0: inactive"])+
#   geom_bar(aes(x=Trips.range, y=Vessels, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 315, hjust=0))+
#   facet_wrap( ~ Country, ncol=5)+
#   labs(title = "Number of SSF vessels by country, vessel length range and number of trips range in area 37", 
#        x = "Number of trips range", 
#        y = "Number of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_by_cou+loa+trips_excl_inactive_area37",".png")
# ggsave(filename=fname, plot=p, width=9, height=6)

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
           by=.(Country_area,Vessel.length.range,Trips.range)]
ot[,Vessels.perc:=nb.Vessels/sum(nb.Vessels,na.rm = T),by=.(Country_area)]
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

countries<-sort(unique(ot$Country_area))

# for(c in countries){
#   p<-ggplot(ot[Country_AREA==c & Trips.range!="0: inactive"])+
#     geom_bar(aes(x=Trips.range, y=nb.Vessels, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#     scale_fill_brewer(palette="Set1")+
#     theme(axis.text.x = element_text(angle = 315, hjust=0))+
#     labs(title = paste0("Number of SSF vessels by LOA range\nand trips range in ",c), 
#          x = "Number of trips range", 
#          y = "Number of vessels",
#          fill="Vessel length range")
#   fname <- paste0("_Ves_by_loa+trips_",c,".png")
#   ggsave(filename=fname, plot=p)
# }

p<-ggplot(ot)+
  geom_bar(aes(x=Trips.range, y=Vessels.perc, fill=factor(Vessel.length.range,levels)),stat = "identity")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 315, hjust=0))+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap( ~ Country_area, ncol=5)+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
              "\n",
              "by country*area, vessel length range and number of trips range"), 
       x = "Number of trips range", 
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_SSFVes_perc_by_cou+vessellengthranges+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=10, height=10)

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("OUI"),]
table(inter$LSF)
table(inter$Scientific_estimates)
inter <- inter[inter$Scientific_estimates %in% c("NON"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range,Trips.range)]
ot[,Vessels.perc:=nb.Vessels/sum(nb.Vessels,na.rm = T),by=.(Country_area)]
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

countries<-sort(unique(ot$Country_area))

# for(c in countries){
#   p<-ggplot(ot[Country_AREA==c & Trips.range!="0: inactive"])+
#     geom_bar(aes(x=Trips.range, y=nb.Vessels, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#     scale_fill_brewer(palette="Set1")+
#     theme(axis.text.x = element_text(angle = 315, hjust=0))+
#     labs(title = paste0("Number of SSF vessels by LOA range\nand trips range in ",c), 
#          x = "Number of trips range", 
#          y = "Number of vessels",
#          fill="Vessel length range")
#   fname <- paste0("_Ves_by_loa+trips_",c,".png")
#   ggsave(filename=fname, plot=p)
# }

p<-ggplot(ot)+
  geom_bar(aes(x=Trips.range, y=Vessels.perc, fill=factor(Vessel.length.range,levels)),stat = "identity")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 315, hjust=0))+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap( ~ Country_area, ncol=5)+
  labs(title = paste("Percentage of LSF declarative data vessels (>=12m)",
                     "\n",
                     "by country*area, vessel length range and number of trips range"), 
       x = "Number of trips range", 
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_LSFVes_perc_by_cou+vessellengthranges+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=10, height=10)

inter<-table3F
table(inter$LSF)
inter <- inter[inter$LSF %in% c("NON"),]
table(inter$LSF)
table(inter[inter$Scientific_estimates %in% c("OUI"),]$Country_area)
table(inter$Scientific_estimates)
inter <- inter[inter$Country_area %in% c("FRA_SSF_GSA7_2019sc","FRA_SSF_GSA7_2019","FRA_SSF_GSA8_2019sc","FRA_SSF_GSA8_2019",
                                         "FRG_SSF_31_2019sc","FRG_SSF_31_2019","FRG_SSF_3141_2019sc","FRG_SSF_3141_2019",
                                         "FRM_SSF_31_2019sc","FRM_SSF_31_2019","FRM_SSF_51_2019sc","FRM_SSF_51_2019",
                                         "FRR_SSF_51_2019sc","FRR_SSF_51_2019","GRC_SSF_GSA22_2019sc","GRC_SSF_GSA22_2019"),]
table(inter$Scientific_estimates)
head(inter)

ot<-inter[Trips.range!="6: inactive" & Trips.range!="7: active" & Trips.range!="8: registered",
          .(nb.Vessels=sum(nb.Vessels,na.rm = T)),
          by=.(Country_area,Vessel.length.range,Trips.range)]
ot[,Vessels.perc:=nb.Vessels/sum(nb.Vessels,na.rm = T),by=.(Country_area)]
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

countries<-sort(unique(ot$Country_area))

# for(c in countries){
#   p<-ggplot(ot[Country_AREA==c & Trips.range!="0: inactive"])+
#     geom_bar(aes(x=Trips.range, y=nb.Vessels, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#     scale_fill_brewer(palette="Set1")+
#     theme(axis.text.x = element_text(angle = 315, hjust=0))+
#     labs(title = paste0("Number of SSF vessels by LOA range\nand trips range in ",c), 
#          x = "Number of trips range", 
#          y = "Number of vessels",
#          fill="Vessel length range")
#   fname <- paste0("_Ves_by_loa+trips_",c,".png")
#   ggsave(filename=fname, plot=p)
# }

p<-ggplot(ot)+
  geom_bar(aes(x=Trips.range, y=Vessels.perc, fill=factor(Vessel.length.range,levels)),stat = "identity")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 315, hjust=0))+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap( ~ Country_area, ncol=5)+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m including scientific estimates)",
                     "\n",
                     "by country*area, vessel length range and number of trips range"), 
       x = "Number of trips range", 
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("Declarative_SSFVes_scientific_perc_by_cou+vessellengthranges+nbtripsranges",".png")
ggsave(filename=fname, plot=p, width=10, height=10)


# ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active",.(nb.Vessels=sum(nb.Vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range,Trips.range)]
# ot[,Vessels.perc:=nb.Vessels/sum(nb.Vessels,na.rm = T),by=.(Country_AREA)]
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)
# 
# p<-ggplot(ot)+
#   geom_bar(aes(x=Trips.range, y=Vessels.perc, fill=factor(Vessel.length.range,levels)),stat = "identity")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 315, hjust=0))+
#   facet_wrap( ~ Country_AREA, ncol=5)+
#   labs(title = "Percentage of SSF vessels by country, vessel length range and number of trips range", 
#        x = "Number of trips range", 
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_perc_by_cou+loa+trips_excl_inactive",".png")
# ggsave(filename=fname, plot=p, width=9, height=6)

# #Registered vs. active by area
# table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
# ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
#            by=.(Country_AREA,Vessel.length.range)]
# ot<-melt(ot, id=c("Country_AREA", "Vessel.length.range"), measure=c("Registered.vessels", "Active.vessels"))
# 
# levels<-sort(unique(ot$Vessel.length.range),decreasing = T)
# p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_AREA)) +
#   geom_bar(stat="identity", width = 0.75, position="dodge")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Percentage of SSF vessels by country*area and vessel length range (registered vs. active)",
#        x = "Country",
#        y = "% of vessels",
#        fill="Vessel length range")
# fname <- paste0("Ves_perc_by_cou+loa",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)

# #Registered vs. active by country
# p<-ggplot(ot,aes(fill=variable,y=value,x=Country_AREA))+
#   geom_bar(stat="identity",position="dodge")+
#   scale_fill_brewer(palette="Set1")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(title = "Number of SSF vessels by country (Area) (registered vs. active)", 
#        x = "Country_AREA", 
#        y = "Number of vessels",
#        fill="")
# fname <- paste0("Ves_by_cou",".png")
# ggsave(filename=fname, plot=p, width=10, height = 5)