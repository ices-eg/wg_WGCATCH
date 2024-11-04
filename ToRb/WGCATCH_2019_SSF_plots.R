library(readxl)
library(data.table)
library(ggplot2)

work.directory<-"C:/Sébastien/Dossiers_courant/_WGCATCH_20151105/2019/TorB2"
setwd(work.directory)
getwd()

#Load the tables
table2<-read_xlsx("SSF_Overwiev_tables_2018_withGreeceEstimated.xlsx",sheet="Sheet1")
table3<-read_xlsx("SSF_Overwiev_tables_2018_withGreeceEstimated.xlsx",sheet="Sheet2")
head(table2)
head(table3)

#Change column names
t2.colnames<-c("Country","Country_code","Supra.region","Area","ICES.region","Year","Vessel.length.range",
               "Registered.vessels","Active.vessels","Status","Others")
t3.colnames<-c("Country","Country_code","Supra.region","Area","ICES.region","Year","Vessel.length.range",
               "Trips.range","nb.Vessels","Status")
colnames(table2)<-t2.colnames
colnames(table3)<-t3.colnames

table2<-data.table(table2)
table(table2$Status)
test<-table2[Status=='ESTIMATED' | Status=='C']
head(test)
table(test$Status)
test<-test[,.(Registered.vessels=sum(Registered.vessels, na.rm=TRUE), Active.vessels=sum(Active.vessels,na.rm=TRUE)), 
           .(Country,Country_code,Area,Vessel.length.range)]
head(test)
table(test$Country)
table(test$Country_code)
table(test$Area)
table(test$Vessel.length.range)
table2<-test 

table3<-data.table(table3)
table(table3$Status)
test<-table3[Status=='ESTIMATED' | Status=='C']
head(test)
table(test$Status)
test<-test[,.(nb.Vessels=sum(nb.Vessels, na.rm=TRUE)), .(Country,Country_code,Area,Vessel.length.range,Trips.range)]
table(test$Country)
table(test$Country_code)
table(test$Area)
table(test$Vessel.length.range)
table3<-test

#Calculate the number of active vessels from table2 and insert into table3
test<-table2
test[, vlr:=paste0(Country,Country_code,Area,Vessel.length.range)]
test<-test[, .(vlr, Active.vessels)]
table3[, vlr:=paste0(Country,Country_code,Area,Vessel.length.range)]
result <- merge(table3,test, all.x=TRUE)
result[, perc:=round(nb.Vessels*100/Active.vessels,2)]
result<-result[,.(Country,Country_code,Area,Vessel.length.range,Trips.range,nb.Vessels,perc)]

table2[,Inactive.vessels:=Registered.vessels-Active.vessels]
inactive<-table2[,.(Country,Country_code,Area,Vessel.length.range,Inactive.vessels)]
inactive[,Trips.range:="6: inactive"]
setnames(inactive,old="Inactive.vessels", new="nb.Vessels")
test<-inactive

inactive<-table2[,.(Country,Country_code,Area,Vessel.length.range,Active.vessels)]
inactive[,Trips.range:="7: active"]
setnames(inactive,old="Active.vessels", new="nb.Vessels")
test<-rbind(test,inactive)

inactive<-table2
inactive[, vlr:=paste0(Country,Country_code,Area,Vessel.length.range)]
inactive<-inactive[, .(vlr, Registered.vessels)]
test[, vlr:=paste0(Country,Country_code,Area,Vessel.length.range)]
test <- merge(test,inactive, all.x=TRUE)
test[, perc:=round(nb.Vessels*100/Registered.vessels,2)]
test<-test[,.(Country,Country_code,Area,Vessel.length.range,Trips.range,nb.Vessels,perc)]

result <- rbind(result,test)

test<-table2[,.(Country,Country_code,Area,Vessel.length.range,Registered.vessels)]
test[,Trips.range:="8: registered"]
setnames(test,old="Registered.vessels", new="nb.Vessels")
test<-unique(test)
test$perc=test$nb.Vessels
test<-test[,c("Country","Country_code","Area","Vessel.length.range","Trips.range","nb.Vessels","perc")]
#test[,Registered.vessels:=NULL]

result <- rbind(result,test)

result[,Id:=paste0(Vessel.length.range," (",Trips.range,")")]

table4<-result[,.(Country,Country_code,Area,Id,nb.Vessels)]
table5<-dcast(table4, Country+Country_code+Area~Id, value.var = "nb.Vessels", fun.aggregate = sum)
#table5[is.na(table5)]<-0
getwd()
write.csv(table5,"No_of_vessels.csv", row.names = FALSE)

table4<-result[,.(Country,Country_code,Area,Id,perc)]
table5<-dcast(table4, Country+Country_code+Area~Id, value.var = "perc", fun.aggregate = sum)
#table5[is.na(table5)]<-0
getwd()
write.csv(table5,"Perc_of_vessels.csv", row.names = FALSE)


#### A FAIRE A FAIRE A FAIRE A FAIRE ##########
### Attribute "NA" à PERC_of_VESSELS si Nb registere vessels = 0 ou  si Nb active vessels = 0 ####
### Attribute "NA" à PERC_of_VESSELS pour Ireland - pas d'info par nb trips_ranges ####
### A FAIRE ####
### Fichier "Perc_of_vessels_ACP.csv" table à utilisé dans les ACPs ####

### changer le nom des colonnes en "0-6_1-9	0-6_10-49	0-6_50-99	0-6_100-149	0-6_p150	0-6_INA	0-6_ACT	0-6_nbvessels	
### 6-8_1-9	6-8_10-49	6-8_50-99	6-8_100-149	6-8_p150	6-8_INA	6-8_ACT	6-8_nbvessels	8-10_1-9	8-10_10-49	8-10_50-99	8-10_100-149	
### 8-10_p150	8-10_INA	8-10_ACT	8-10_nbvessels	10-12_1-9	10-12_10-49	10-12_50-99	10-12_100-149	10-12_p150	10-12_INA	
### 10-12_ACT	10-12_nbvessels
### A FAIRE A FAIRE ################################

###################### ICI ICI ICI ICI ICI ICI ICI ICI ICI ICI ICI #############################
###################### 02-12-2019 ##############################################################

# https://thinkr.fr/pdf/ggplot2-french-cheatsheet.pdf

#Registered vs. active by country
table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),
              Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_AREA)]
ot<-melt(ot, id=c("Country_AREA"), measure=c("Registered.vessels", "Declarative.data.vessels"))
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_AREA))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area",
                     "\n",
                     "(declarative data vessels vs registered)"), 
       x = "Country_AREA", 
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)

p<-ggplot(ot[!(ot$Country %in% c("GRC_37")),],aes(fill=variable,y=value,x=Country_AREA))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area",
               "\n",
               "(declarative data vessels vs registered)"), 
       x = "Country_AREA", 
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou2",".png")
ggsave(filename=fname, plot=p, width=10, height = 5)


#Perc. registered vs. vessels with declarative data by country*area and vessel length ranges
table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_AREA,Vessel.length.range)]

test<-table2[,.(nb.vessels=sum(Registered.vessels,na.rm = T)),
             by=.(Country_AREA)]
head(test)
dim(ot)
ot<-merge(ot,test,by=c("Country_AREA"))
dim(ot)
head(ot)
ot$perc.Registered.vessels <- (ot$Registered.vessels/ot$nb.vessels)*100
ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$nb.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_AREA", "Vessel.length.range"), measure=c("perc.Registered.vessels", "perc.Declarative.data.vessels"))
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(fill=factor(Vessel.length.range,levels), y=value, x=Country_AREA)) +
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

#Perc. declarative data vessels by country*area and vessel length ranges
table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Active.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_AREA,Vessel.length.range)]

ot$perc.Declarative.data.vessels <- (ot$Active.vessels/ot$Registered.vessels)*100
head(ot)

ot<-melt(ot, id=c("Country_AREA", "Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
#levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

p<-ggplot(ot, aes(y=value, x=Vessel.length.range)) +
  geom_bar(stat="identity", width = 0.75, position="stack")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Country_AREA)+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF vessels (<12m) with declarative data vs SSF registered vessels",
                     "\n",
                     "by country*area and vessel length range"),
       x = "Vessel length ranges",
       y = "% of SSF vessels (<12m) with declarative data vs SSF registered vessels")
fname <- paste0("Declarative_vs_Registered_SSFVes_perc_by_cou+loa2",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

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
table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_AREA,Vessel.length.range)]
ot<-melt(ot, id=c("Country_AREA","Vessel.length.range"), measure=c("Registered.vessels", "Declarative.data.vessels"))
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_AREA))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country_AREA",
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)

p<-ggplot(ot[ot$Country_AREA != "GRC_37",],aes(fill=variable,y=value,x=Country_AREA))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set1")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Number of SSF vessels (<12m) by country*area and vessel length range",
                     "\n",
                     "(declarative data vessels vs registered)"),
       x = "Country_AREA",
       y = "Number of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_nb_by_cou2+loa",".png")
ggsave(filename=fname, plot=p, width=10, height = 10)


#Registered vs. active by country by vessel length ranges
table2$Country_AREA <- paste(table2$Country_code, table2$Area,sep="_")
ot<-table2[,.(Registered.vessels=sum(Registered.vessels,na.rm = T),Declarative.data.vessels=sum(Active.vessels,na.rm = T)),
           by=.(Country_AREA,Vessel.length.range)]
ot$perc.Declarative.data.vessels <- ot$Declarative.data.vessels/ot$Registered.vessels
ot<-melt(ot, id=c("Country_AREA","Vessel.length.range"), measure=c("perc.Declarative.data.vessels"))
p<-ggplot(ot,aes(fill=variable,y=value,x=Country_AREA))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Set2")+
  facet_wrap(.~Vessel.length.range)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
               "\n",
               " by country*area and vessel length range"), 
       x = "Country_AREA", 
       y = "%r of vessels",
       fill="")
fname <- paste0("Declarative_vs_Registered_SSFVes_perc_by_cou2+loa",".png")
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
ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active",.(nb.Vessels=sum(nb.Vessels,na.rm = T)),
           by=.(Country_AREA,Trips.range)]

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

p<-ggplot(ot, aes(fill=factor(Trips.range,levels), y=nb.Vessels, x=Country_AREA)) +
  geom_bar(stat="identity", width = 0.75, position="fill")+
  scale_fill_brewer(palette="Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
              "\n",
              "by country*area and number of trips range"), 
       x = "Country_AREA", 
       y = "% of vessels",
       fill="Number of trips range")
fname <- paste0("declarative_SSFVes_perc_by_cou+nbtripsranges",".png")
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


ot<-table3[Trips.range!="6: inactive" & Trips.range!="7: active",.(nb.Vessels=sum(nb.Vessels,na.rm = T)),
           by=.(Country_AREA,Vessel.length.range,Trips.range)]
ot[,Vessels.perc:=nb.Vessels/sum(nb.Vessels,na.rm = T),by=.(Country_AREA)]
levels<-sort(unique(ot$Vessel.length.range),decreasing = T)

countries<-sort(unique(ot$Country_AREA))

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
  facet_wrap( ~ Country_AREA, ncol=5)+
  labs(title = paste("Percentage of SSF declarative data vessels (<12m)",
              "\n",
              "by country*area, vessel length range and number of trips range"), 
       x = "Number of trips range", 
       y = "% of vessels",
       fill="Vessel length range")
fname <- paste0("declarative_SSFVes_perc_by_cou+vessellengthranges+nbtripsranges",".png")
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