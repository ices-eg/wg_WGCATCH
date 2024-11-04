################################################################################################################
# Auteur: Sébastien Demanèche
# FLeet WGCATCH
# Version "fleet-export.zip" from https://ec.europa.eu/fisheries/cfp/fishing_rules/fishing_fleet_en v20191105
################################################################################################################

getwd()          #Nom du répertoire de travail de R
dir()              #Liste des fichiers du répertoire
rm(list = ls())    #Commande permettant d'effacer tous les variables
graphics.off()	 #Efface tous les graphics en mémoire

path = "C:/Sébastien/Dossiers_courant/_WGCATCH_20151105/2019/TorB2"
setwd(path)

memory.limit(size = 100000)
memory.limit()
library (lubridate)
cleanMem <- function(n=10) { for (i in 1:n) gc() }
getwd()

#----------------------------------------------------------------------------#
#            LECTURE TABLE %                                                 #
#----------------------------------------------------------------------------#

path_file = paste ("./Perc_of_vessels_ACP.csv",sep="")
path_file
TABLE <- read.csv(path_file,head=TRUE,sep=";",dec=".")
head(TABLE)
dim(TABLE)

TABLE$Country_AREA <- paste(TABLE$Country_code, TABLE$Area,sep="_")
head(TABLE)
row.names(TABLE) <- TABLE$Country_AREA

head(TABLE)
colnames(TABLE)
TABLE_ACP1 <- TABLE[,c(36,10,18,26,34)]
row.names(TABLE_ACP1) <- TABLE_ACP1$Country_AREA
head(TABLE_ACP1)

library(FactoMineR)
res.pca_TABLE1 = PCA(TABLE_ACP1,scale.unit=F,ncp=5,graph=T,quali.sup=c(1))
Table1_ACP <- data.frame(res.pca_TABLE1$var$cor)
head(Table1_ACP)
Table1_ACP[order(-Table1_ACP$Dim.1),]
Table1_ACP[order(-Table1_ACP$Dim.2),]
res.pca_TABLE1$eig
res.hcpc_TABLE1 = HCPC(res.pca_TABLE1)
res.hcpc_TABLE1$desc.var
plot.PCA(res.pca_TABLE1, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE1,axes=c(1,2),choice="map",draw.tree=FALSE)

colnames(TABLE)
TABLE_ACP2 <- TABLE[,c(36,4:8,12:16,20:24,28:32)]
row.names(TABLE_ACP2) <- TABLE_ACP2$Country_AREA

library(FactoMineR)
res.pca_TABLE2 = PCA(TABLE_ACP2,scale.unit=F,ncp=5,graph=T,quali.sup=c(1))
Table2_ACP <- data.frame(res.pca_TABLE2$var$cor)
head(Table2_ACP)
Table2_ACP[order(-Table2_ACP$Dim.1),]
Table2_ACP[order(-Table2_ACP$Dim.2),]
res.pca_TABLE2$eig
res.hcpc_TABLE2 = HCPC(res.pca_TABLE2)
res.hcpc_TABLE2$desc.var
plot.PCA(res.pca_TABLE2, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE2,axes=c(1,2),choice="map",draw.tree=FALSE)

colnames(TABLE)
TABLE_ACP3 <- TABLE[,c(36,4:8,10,12:16,18,20:24,26,28:32,34)]
row.names(TABLE_ACP3) <- TABLE_ACP3$Country_AREA

library(FactoMineR)
res.pca_TABLE3 = PCA(TABLE_ACP3,scale.unit=F,ncp=5,graph=T,quali.sup=c(1))
Table3_ACP <- data.frame(res.pca_TABLE3$var$cor)
head(Table3_ACP)
Table3_ACP[order(-Table3_ACP$Dim.1),]
Table3_ACP[order(-Table3_ACP$Dim.2),]
res.pca_TABLE3$eig
res.hcpc_TABLE3 = HCPC(res.pca_TABLE3)
res.hcpc_TABLE3$desc.var
plot.PCA(res.pca_TABLE3, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE3,axes=c(1,2),choice="map",draw.tree=FALSE)