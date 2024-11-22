library(openxlsx)
library(data.table)
library(ggplot2)
library(countrycode)

rm(list=ls())
gc()

qFiles<-list.files("ToRb/questionnaires", full.names = TRUE)

vesselsColnames <- c("CountryArea","SupraRegionArea","GeoIndicator","Year",
                     "VesselLengthRange","NumberOfRegisteredVessels",
                     "NumberOfActiveVessels","Comments")
tripsColnames <- c("CountryArea","SupraRegionArea","GeoIndicator","Year",
                   "VesselLengthRange","NumberOfTripsRange",
                   "NumberOfVessels")


qDataVessels <- do.call(rbind,lapply(qFiles,function(x){
  read.xlsx(x, sheet = "No_vessels", startRow = 8,
            skipEmptyCols = FALSE)
}))
qDataTrips <- do.call(rbind,lapply(qFiles,function(x){
  read.xlsx(x, sheet = "No_vessels_per_trip", 
            startRow = ifelse(x %in% c("ToRb/questionnaires/SSF_data-quality risk assessment methodology _questionnaire_WGCATCH2024_BasqueC-AZTI.xlsx",
                                       "ToRb/questionnaires/SSF_data-quality risk assessment methodology _questionnaire_WGCATCH2024_POL.xlsx"),
                              7,8),
            skipEmptyCols = FALSE)
}))

colnames(qDataVessels) <- vesselsColnames
colnames(qDataTrips) <- tripsColnames
qDataVessels <- data.table(qDataVessels)
qDataTrips <- data.table(qDataTrips)

###########
# Vessels #
###########
# Country codes
table(qDataVessels$CountryArea, useNA = "ifany")
qDataVessels[CountryArea == "DNK/Denmark",CountryArea:="DNK"]
qDataVessels[CountryArea == "SWE/Sweden",CountryArea:="SWE"]
qDataVessels[CountryArea == "LTU/Lithuania",CountryArea:="LTU"]
qDataVessels[CountryArea == "FRA/France",CountryArea:="FRA"]
qDataVessels[CountryArea == "ES_IEO",CountryArea:="ESP_IEO"]
qDataVessels[CountryArea == "PRT/Portugal" & GeoIndicator == "P3",
      CountryArea:="PRT_Azores"]
qDataVessels[CountryArea == "NL",CountryArea:="NLD"]
qDataVessels[CountryArea == "SCO/Scotland",CountryArea:="SCO"]
table(qDataVessels$CountryArea)
qDataVessels[,Country:=substr(CountryArea,1,3)]
table(qDataVessels$Country, useNA = "ifany")

# Supra regions
table(qDataVessels$SupraRegionArea, useNA = "ifany")
qDataVessels[SupraRegionArea %in% c("AREA27","Baltic Sea","FAO area 27","ICES","NAO",
                         "NAO, all Areas (not Baltic Sea)","NAO, Area 27 (not Baltic Sea)",
                         "NOR","27","FAO Area 27"),
      SupraRegion:="NAO"]
qDataVessels[SupraRegionArea %in% c("MBS"),
      SupraRegion:="MBS"]
qDataVessels[SupraRegionArea %in% c("OFR","OFR, Area 87"),
      SupraRegion:="OFR"]
table(qDataVessels$SupraRegion, useNA = "ifany")

# GEO indicators
table(qDataVessels$GeoIndicator, useNA = "ifany")

# Years of reference
table(qDataVessels$Year)

# Vessels length ranges
table(qDataVessels$VesselLengthRange, useNA = "ifany")
qDataVessels[VesselLengthRange %in% c("[0-6[m","[0-6[m ","0-6"),VesselLengthRange:="0-6m"]
qDataVessels[VesselLengthRange %in% c("[6-8[m","6-8"),VesselLengthRange:="6-8m"]
qDataVessels[VesselLengthRange %in% c("[8-10[m","8-10"),VesselLengthRange:="8-10m"]
qDataVessels[VesselLengthRange %in% c("[10-12[m","10-12"),VesselLengthRange:="10-12m"]
#qDataVessels[VesselLengthRange %in% c("6-12"),VesselLengthRange:="6-12m*"]
qDataVessels[VesselLengthRange %in% c("[12-15[m","12-15"),VesselLengthRange:="12-15m"]
#qDataVessels[VesselLengthRange %in% c("12-18"),VesselLengthRange:="12-18m*"]
qDataVessels[VesselLengthRange %in% c("[15-18[m","15-18"),VesselLengthRange:="15-18m"]
qDataVessels[VesselLengthRange %in% c("[18-24[m","18-24"),VesselLengthRange:="18-24m"]
qDataVessels[VesselLengthRange %in% c("[24-40[m","24-40"),VesselLengthRange:="24-40m"]
qDataVessels[VesselLengthRange %in% c(">=40m"),VesselLengthRange:="40m and more"]
table(qDataVessels$VesselLengthRange, useNA = "ifany")

# Number of vessels check
dataToPlot<-qDataVessels[,.(NumberOfRegisteredVessels=sum(NumberOfRegisteredVessels, na.rm = T),
         NumberOfActiveVessels=sum(NumberOfActiveVessels, na.rm = T)),
      by = .(Country)]
dataToPlot<-melt(dataToPlot,id.vars = "Country",
                 measure.vars = c("NumberOfRegisteredVessels","NumberOfActiveVessels"))

p<-ggplot(data = dataToPlot)+
  geom_bar(aes(x=Country, y=value, fill=variable),
           stat="identity", position="dodge")+
  theme_bw()+
  scale_y_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = "Paired")+
  labs(x="Country", y="Number of vessels",fill="Registered/Active")
p
#########
# Trips #
#########

# Country codes
table(qDataTrips$CountryArea, useNA = "ifany")
qDataTrips[CountryArea == "DNK/Denmark",CountryArea:="DNK"]
qDataTrips[CountryArea == "SWE/Sweden",CountryArea:="SWE"]
qDataTrips[CountryArea == "LTU/Lithuania",CountryArea:="LTU"]
qDataTrips[CountryArea == "FRA/France",CountryArea:="FRA"]
qDataTrips[CountryArea == "PRT/Portugal" & GeoIndicator == "P3",
             CountryArea:="PRT_Azores"]
qDataTrips[CountryArea == "NL",CountryArea:="NLD"]
qDataTrips[CountryArea == "SCO/Scotland",CountryArea:="SCO"]
table(qDataTrips$CountryArea, useNA = "ifany")
qDataTrips[,Country:=substr(CountryArea,1,3)]
table(qDataTrips$Country, useNA = "ifany")

# Supra regions
table(qDataTrips$SupraRegionArea, useNA = "ifany")
qDataTrips[SupraRegionArea %in% c("AREA27","Baltic Sea","FAO area 27","ICES","NAO",
                                    "NAO, all Areas (not Baltic Sea)","NAO, Area 27 (not Baltic Sea)",
                                  "27","FAO27","FAO Area 27"),
             SupraRegion:="NAO"]
qDataTrips[SupraRegionArea %in% c("MBS"),
             SupraRegion:="MBS"]
qDataTrips[SupraRegionArea %in% c("OFR","OFR, Area 87"),
             SupraRegion:="OFR"]
qDataTrips[CountryArea %in% c("NOR","ESP_BC"),
           SupraRegion:="NAO"]
table(qDataTrips$SupraRegion, useNA = "ifany")

# GEO indicators
table(qDataTrips[,.(Country,GeoIndicator)], useNA = "ifany")

# Years of reference
table(qDataTrips$Year, useNA = "ifany")

# Vessels length ranges
table(qDataTrips$VesselLengthRange, useNA = "ifany")
qDataTrips[VesselLengthRange %in% c("[0-6[m","[0-6[m ","0-6"),VesselLengthRange:="0-6m"]
qDataTrips[VesselLengthRange %in% c("[6-8[m","6-8"),VesselLengthRange:="6-8m"]
qDataTrips[VesselLengthRange %in% c("[8-10[m","8-10"),VesselLengthRange:="8-10m"]
qDataTrips[VesselLengthRange %in% c("[10-12[m","10-12"),VesselLengthRange:="10-12m"]
#qDataTrips[VesselLengthRange %in% c("6-12"),VesselLengthRange:="6-12m*"]
qDataTrips[VesselLengthRange %in% c("[12-15[m","12-15"),VesselLengthRange:="12-15m"]
#qDataTrips[VesselLengthRange %in% c("12-18"),VesselLengthRange:="12-18m*"]
qDataTrips[VesselLengthRange %in% c("[15-18[m","15-18"),VesselLengthRange:="15-18m"]
qDataTrips[VesselLengthRange %in% c("[18-24[m","18-24"),VesselLengthRange:="18-24m"]
qDataTrips[VesselLengthRange %in% c("[24-40[m","24-40"),VesselLengthRange:="24-40m"]
qDataTrips[VesselLengthRange %in% c(">=40m"),VesselLengthRange:="40m and more"]
table(qDataTrips$VesselLengthRange)

# Trips ranges
table(qDataTrips$NumberOfTripsRange, useNA = "ifany")
qDataTrips[NumberOfTripsRange %in% c("<10","<10 trips"),NumberOfTripsRange:="<10 trips"]
qDataTrips[NumberOfTripsRange %in% c("[10-50[ trips","10-50","10—50"),NumberOfTripsRange:="10-50 trips"]
qDataTrips[NumberOfTripsRange %in% c("[50-100[ trips"," [50-100[ trips","50-100"),NumberOfTripsRange:="50-100 trips"]
qDataTrips[NumberOfTripsRange %in% c("[100-150[ trips","100-150"),NumberOfTripsRange:="100-150 trips"]
qDataTrips[NumberOfTripsRange %in% c(">=150 trips",">=150"," >=150 trips"),NumberOfTripsRange:=">=150 trips"]
table(qDataTrips$NumberOfTripsRange, useNA = "ifany")

# Compare tables
dataToCompare1 <- qDataVessels[,.(ActiveTable1=sum(NumberOfActiveVessels, na.rm = T)),
                           by=.(Country)]
dataToCompare2 <- qDataTrips[,.(ActiveTable2=sum(NumberOfVessels, na.rm = T)),
                           by=.(Country)]
dataToCompare <- merge(dataToCompare1, dataToCompare2, by="Country")
dataToCompare[,Diff:=ActiveTable1 - ActiveTable2]

# Formatted output for Sebastian's script
table1 <- qDataVessels
table1$CountryName <- countrycode(table1$Country,origin = "iso3c",
                                  destination = "country.name")
table1[Country=="GBE", CountryName:="England"]
table1[Country=="SCO", CountryName:="Scotland"]
table(table1$CountryName,useNA = "ifany")
table1$Year<-2023
table1$YearMax <- "OUI"
table1[,LSF:=ifelse(VesselLengthRange %in% c("12-15m","15-18m","18-24m",
                                             "24-40m","40m and more","12-18m*"),
                    "OUI","NON")]
table(table1[,.(VesselLengthRange,LSF)],useNA = "ifany")
table1[,CountryAreaId:=ifelse(is.na(GeoIndicator),
                              paste(Country,
                                    ifelse(LSF=="OUI","LSF","SSF"),
                                    SupraRegion,
                                    Year,
                                    sep="_"),
                              paste(Country,
                                    ifelse(LSF=="OUI","LSF","SSF"),
                                    SupraRegion,
                                    GeoIndicator,
                                    Year,
                                    sep="_"))]
table1[VesselLengthRange=="0-6m",VesselLengthRange:="1: 0-6"]
table1[VesselLengthRange=="6-8m",VesselLengthRange:="2: 6-8"]
table1[VesselLengthRange=="8-10m",VesselLengthRange:="3: 8-10"]
table1[VesselLengthRange=="10-12m",VesselLengthRange:="4: 10-12"]
table1[VesselLengthRange=="12-15m",VesselLengthRange:="5: 12-15"]
table1[VesselLengthRange=="15-18m",VesselLengthRange:="6: 15-18"]
table1[VesselLengthRange=="18-24m",VesselLengthRange:="7: 18-24"]
table1[VesselLengthRange=="24-40m",VesselLengthRange:="8: 24-XX"]
table1[VesselLengthRange=="40m and more",VesselLengthRange:="8: 24-XX"]
#table1[VesselLengthRange=="6-12m*",VesselLengthRange:="*: 6-12"]
#table1[VesselLengthRange=="12-18m*",VesselLengthRange:="*: 12-18"]
table1$ScientificEstimates<-"NON"
table1$ICESregion <- ""


table1 <- table1[,.(NumberOfRegisteredVessels=sum(NumberOfRegisteredVessels, na.rm = T),
                    NumberOfActiveVessels=sum(NumberOfActiveVessels, na.rm = T)),
                 by=.(CountryName, Country, SupraRegion,
                      ICESregion, CountryAreaId, Year, YearMax, 
                      VesselLengthRange, ScientificEstimates,LSF)]
table1 <- table1[,.(CountryName, Country, SupraRegion, SupraRegion,
                    ICESregion, CountryAreaId, Year, YearMax, 
                    VesselLengthRange, NumberOfRegisteredVessels,
                    NumberOfActiveVessels, ScientificEstimates,LSF)]
table1$Year <- as.numeric(table1$Year)
table1 <- table1[NumberOfRegisteredVessels != 0 |
                   NumberOfActiveVessels != 0]

table2 <- qDataTrips
table2$CountryName <- countrycode(table2$Country,origin = "iso3c",
                                  destination = "country.name")
table2[Country=="GBE", CountryName:="England"]
table2[Country=="SCO", CountryName:="Scotland"]
table2$Year<-2023
table2$YearMax <- "OUI"
table2[,LSF:=ifelse(VesselLengthRange %in% c("12-15m","15-18m","18-24m",
                                             "24-40m","40m and more","12-18m*"),
                    "OUI","NON")]
table(table2[,.(VesselLengthRange,LSF)],useNA = "ifany")
table2[,CountryAreaId:=ifelse(is.na(GeoIndicator),
                              paste(Country,
                                    ifelse(LSF=="OUI","LSF","SSF"),
                                    SupraRegion,
                                    Year,
                                    sep="_"),
                              paste(Country,
                                    ifelse(LSF=="OUI","LSF","SSF"),
                                    SupraRegion,
                                    GeoIndicator,
                                    Year,
                                    sep="_"))]
table2[VesselLengthRange=="0-6m",VesselLengthRange:="1: 0-6"]
table2[VesselLengthRange=="6-8m",VesselLengthRange:="2: 6-8"]
table2[VesselLengthRange=="8-10m",VesselLengthRange:="3: 8-10"]
table2[VesselLengthRange=="10-12m",VesselLengthRange:="4: 10-12"]
table2[VesselLengthRange=="12-15m",VesselLengthRange:="5: 12-15"]
table2[VesselLengthRange=="15-18m",VesselLengthRange:="6: 15-18"]
table2[VesselLengthRange=="18-24m",VesselLengthRange:="7: 18-24"]
table2[VesselLengthRange=="24-40m",VesselLengthRange:="8: 24-XX"]
table2[VesselLengthRange=="40m and more",VesselLengthRange:="8: 24-XX"]
#table2[VesselLengthRange=="6-12m*",VesselLengthRange:="*: 6-12"]
#table2[VesselLengthRange=="12-18m*",VesselLengthRange:="*: 12-18"]
table2$ScientificEstimates<-"NON"
table2$ICESregion <- ""

table2[NumberOfTripsRange=="<10 trips",NumberOfTripsRange:="1: 1-9"]
table2[NumberOfTripsRange=="10-50 trips",NumberOfTripsRange:="2: 10-49"]
table2[NumberOfTripsRange=="50-100 trips",NumberOfTripsRange:="3: 50-99"]
table2[NumberOfTripsRange=="100-150 trips",NumberOfTripsRange:="4: 100-149"]
table2[NumberOfTripsRange==">=150 trips",NumberOfTripsRange:="5: >=150"]
table(table2$NumberOfTripsRange, useNA = "ifany")

table2 <- table2[,.(NumberOfVessels=sum(NumberOfVessels,na.rm = T)),
                 by=.(CountryName, Country, SupraRegion,
                      ICESregion, CountryAreaId, Year, YearMax, 
                      VesselLengthRange, NumberOfTripsRange, ScientificEstimates,LSF)]
table2 <- table2[,.(CountryName, Country, SupraRegion, SupraRegion,
                    ICESregion, CountryAreaId, Year, YearMax, 
                    VesselLengthRange, NumberOfTripsRange,
                    NumberOfVessels, ScientificEstimates,LSF)]
table2 <- table2[NumberOfVessels!=0]

wb<-createWorkbook("WGCATCH_2024")
addWorksheet(wb,"Sheet1")
writeData(wb,"Sheet1",table1)
addWorksheet(wb,"Sheet2")
writeData(wb,"Sheet2",table2)
saveWorkbook(wb, file="ToRb/data_combined/WGCATCH_2024_data.xlsx", overwrite = T)
# The above file will be the input for the script 'WGCATCH_2020_SSF_plotsF.R'
