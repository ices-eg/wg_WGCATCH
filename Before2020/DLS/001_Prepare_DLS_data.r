# ==================================
# Reads and Prepares DLS data
# Nuno, Sven, Kirsten, Karolina @ WGCATCH 2018-2019
# ==================================


# Note: Input files are 
    # "WGXXX_YYYY_Compilation.xlsx" - compilation from questionnaires sent by the different countries
    # "DLS Summary.txt" - the one existing should suffice - probably enough just give it a check and update


rm(list=ls())

# loads packages
    library(xlsx)

# specify project you are working on
    project <- "WGBFAS_DLS_2018"
    #input_file <- "WGBFAS_flatfish_2019"
    #input_file <- "WGNSSK_sol274_2019"

 # directory structure   
    dir_inputs <- "001_Inputs/"
    dir_inputs_project <- paste(dir_inputs, project,"/", sep="")
 

# loads compiled data
    df0<-read.xlsx(paste(dir_inputs_project, project, "_Compilation.xlsx", sep=""), sheetIndex=1, startRow=2, colIndex=1:17)
    dls_tab<-read.table(paste(dir_inputs, "DLS Summary.txt", sep=""), sep="\t", header=T)

# data cleaning routines
    
    df1<-df0
    
    sum(duplicated(df0))
    df1<-df1[!duplicated(df1),]

    colnames(df1)<-c("country","species","faoCode","stockCode","area","fleet","year","quarter","catchCateg","totVol","totTrips","totTripsSpp","totTripsObs","totTripsObsSpp","totTripsObsSppLt","totFishMeas","totFishWeight")

    head(df1)
    summary(df1)

    table(df1$species, useNA="al")
    df1$species<-tolower(df1$species)
    df1[df1$species == "lophiidae (lophius budegassa + lophius piscatorius)","species"]<-"lophiidae"
    df1[df1$species == "pleuronectes platessus","species"]<-"pleuronectes platessa"
    df1[df1$species == "anglerfish","species"]<-"lophiidae"
    df1[df1$species == "solea solea (=s.vulgaris)","species"]<-"solea solea"
    df1[df1$species == "scophthalmus maximus (=psetta maxima)","species"]<-"scophthalmus maximus"
    df1[df1$species == "solea solea ","species"]<-"solea solea"
    df1[df1$species == "blue ling","species"]<-"molva dypterygia"
    df1[df1$species == "ling","species"]<-"molva molva"
    df1[df1$species == "leucorajs naevus","species"]<-"leucoraja naevus"
    df1[df1$species == "leucoraja naevus ","species"]<-"leucoraja naevus"
    df1[df1$species == "raja clavata ","species"]<-"raja clavata"
    df1[df1$species == "scopthalmus rhombus","species"]<-"scophthalmus rhombus"
    df1[df1$species == "scopthalmus maximus","species"]<-"scophthalmus maximus"
    df1[df1$species == "psetta maxima","species"]<-"scophthalmus maximus"
    df1[df1$species == "plathichthys flesus","species"]<-"platichthys flesus"
    df1<-df1[!df1$species == "remap this to rox then delete this",]
    table(df1$species, useNA="al")

    table(df1$faoCode, useNA="al")
    df1$faoCode<-toupper(df1$faoCode)
    table(df1$faoCode, useNA="al")

    df1$stockCode<-tolower(df1$stockCode)
    table(df1$stockCode, useNA="al")
    df1$DLS<-dls_tab$DLS[match(df1$stockCode, dls_tab$Stock)]
    table(df1$stockCode, df1$DLS, useNA="al")
    table(df1$stockCode[is.na(df1$DLS)])

    table(df1$country, df1$DLS, useNA="al")


    table(df1$area, useNA="al")
    df1$area <- as.character(df1$area)
    df1[df1$area == "3as","area"]<-"27.3.a.21"
    df1[df1$area == "22","area"]<-"27.3.c.22"
    df1[df1$area == "23","area"]<-"27.3.b.23"
    df1[df1$area == "24","area"]<-"27.d.24"
    df1[df1$area == "25","area"]<-"27.d.25"
    df1[df1$area == "26","area"]<-"27.d.26"
    df1[df1$area == "27","area"]<-"27.d.27"
    df1[df1$area == "28","area"]<-"27.d.28"
    df1[df1$area == "6A","area"]<-"27.6.a"
    df1[df1$area == "7B","area"]<-"27.7.b"
    df1[df1$area == "7C","area"]<-"27.7.c"
    df1[df1$area == "7G","area"]<-"27.7.g"
    df1[df1$area == "7H","area"]<-"27.7.h"
    df1[df1$area == "7J","area"]<-"27.7.j"
    df1[df1$area == "7K","area"]<-"27.7.k"
    df1[df1$area == "8A","area"]<-"27.8.a"
    df1[df1$area == "8B","area"]<-"27.8.b"
    df1[df1$area == "8C","area"]<-"27.8.c"
    df1[df1$area == "8D","area"]<-"27.8.d"
    table(df1$area, useNA="al")

    table(df1$catchCateg, useNA="al")
    df1[df1$catchCateg=="LAN","catchCateg"]<-"L"

    df1$ID<-apply(df1[,1:9], 1, paste, collapse="_")
    df1$ID[1]


    df1$totVol<-as.numeric(as.character(df1$totVol))
    df1$totTrips<-as.numeric(as.character(df1$totTrips))
    df1$totTripsSpp<-as.numeric(as.character(df1$totTripsSpp))
    df1$totTripsObs<-as.numeric(as.character(df1$totTripsObs))
    df1$totTripsObsSpp<-as.numeric(as.character(df1$totTripsObsSpp))
    df1$totTripsObsSppLt<-as.numeric(as.character(df1$totTripsObsSppLt))
    df1$totFishMeas<-as.numeric(as.character(df1$totFishMeas))
    df1$totFishWeight<-as.numeric(as.character(df1$totFishWeight))

    df1$totTripsObsClass <- cut(df1$totTripsObs, breaks = c(-1,0,1,2,3,5,10,30,50,100,200,300,400,99999999), include.lowest =T, labels = c("0","1","2","3","4-5","6-10","11-30","31-50","51-100","101-200","201-300","301-400",">400"))
    df1$totTripsObsSppClass<-cut(df1$totTripsObsSpp, breaks = c(-1,0,1,2,3,5,10,30,50,100,200,300,400,99999999), include.lowest =T, labels = c("0","1","2","3","4-5","6-10","11-30","31-50","51-100","101-200","201-300","301-400",">400"))
    df1$totTripsObsSppLtClass <- cut(df1$totTripsObsSppLt, breaks = c(-1,0,1,2,3,5,10,30,50,100,200,300,400,99999999), include.lowest =T, labels = c("0","1","2","3","4-5","6-10","11-30","31-50","51-100","101-200","201-300","301-400",">400"))
    df1$totFishMeasClass<-cut(df1$totFishMeas, breaks = c(-1,0, 1,2,3,5,10,30,50,100,200,300,400,500, 1000, 5000, 10000, 99999999), include.lowest =T, labels = c("0","1","2","3","4-5","6-10","11-30","31-50","51-100","101-200","201-300","301-400","400-500","501-1000","1001-5000","5001-10000",">10000"))

# saves data clean/prepared
    save(df1, dls_tab, file=paste(dir_inputs_project, "001_data_prepared_",project,".Rdata", sep=""))


