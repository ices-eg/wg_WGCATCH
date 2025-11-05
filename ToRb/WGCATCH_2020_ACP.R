################################################################################################################
# Auteur: Sébastien Demanèche
# FLeet WGCATCH
# Version "fleet-export.zip" from https://ec.europa.eu/fisheries/cfp/fishing_rules/fishing_fleet_en v20191105
################################################################################################################

getwd()          #Nom du répertoire de travail de R
dir()              #Liste des fichiers du répertoire
rm(list = ls())    #Commande permettant d'effacer tous les variables
graphics.off()	 #Efface tous les graphics en mémoire

path = "C:/Sebastien/Dossiers_courant/_WGCATCH_20151105/2025/QUESTIONNAIRES_ANSWERS/Analysis_2020/test"
setwd(path)

memory.limit(size = 100000)
memory.limit()
library (lubridate)
cleanMem <- function(n=10) { for (i in 1:n) gc() }
getwd()

#----------------------------------------------------------------------------#
#            LECTURE TABLE %                                                 #
#----------------------------------------------------------------------------#

path_file = paste ("./Perc_of_vessels_complet.csv",sep="")
path_file
TABLE <- read.csv(path_file,head=TRUE,sep=",",dec=".")
head(TABLE)
dim(TABLE)
table(TABLE$Country_area)

TABLE$ind.sup <- "NON"
TABLE[TABLE$Year_max == "NON",]$ind.sup <- "OUI"
TABLE[TABLE$Scientific_estimates == "OUI",]$ind.sup <- "OUI"
TABLE[TABLE$LSF == "OUI",]$ind.sup <- "OUI"
TABLE <- TABLE[order(TABLE$ind.sup,TABLE$Country_area),]
table(TABLE$ind.sup)
unique(TABLE[TABLE$ind.sup == "NON",c("Country_area")])
unique(TABLE[TABLE$ind.sup == "OUI",c("Country_area")])

#TABLE$Country_AREA <- paste(TABLE$Country_code, TABLE$Area,sep="_")
#head(TABLE)
#row.names(TABLE) <- TABLE$Country_AREA

################# PCA1
head(TABLE)
colnames(TABLE)
TABLE_ACP1 <- TABLE[,c(2,12,20,28,36)]
row.names(TABLE_ACP1) <- TABLE_ACP1$Country_area
head(TABLE_ACP1)
dim(TABLE_ACP1)

library(FactoMineR)
library(factoextra)
res.pca_TABLE1 = PCA(TABLE_ACP1,scale.unit=F,ncp=5,graph=T,quali.sup=c(1),ind.sup=c(33:92))
Table1_ACP <- data.frame(res.pca_TABLE1$var$cor)
head(Table1_ACP)
Table1_ACP[order(-Table1_ACP$Dim.1),]
Table1_ACP[order(-Table1_ACP$Dim.2),]
res.pca_TABLE1$eig
res.hcpc_TABLE1 = HCPC(res.pca_TABLE1)
res.hcpc_TABLE1$desc.var
plot.PCA(res.pca_TABLE1, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot.PCA(res.pca_TABLE1, axes=c(1,2), choix="ind",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE1,axes=c(1,2),choice="map",draw.tree=FALSE)

fviz_cluster(res.hcpc_TABLE1,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             #palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                         "#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606"),
             ggtheme = theme_minimal(),
             main = "Factor map"
)

fviz_dend(res.hcpc_TABLE1, 
          cex = 0.7,                     # Taille du text
          #palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                      "#25FDE9", "#DF73FF", "#9E9E9E"),
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = c("#000000"),           # Couleur du rectangle
          labels_track_height = 500      # Augment l'espace pour le texte
)

#########################################################################################

################# PCA2
head(TABLE)
colnames(TABLE)
TABLE_ACP2 <- TABLE[,c(2,6:10,14:18,22:26,30:34)]
row.names(TABLE_ACP2) <- TABLE_ACP2$Country_area
head(TABLE_ACP2)
dim(TABLE_ACP2)

library(FactoMineR)
library(factoextra)
res.pca_TABLE2 = PCA(TABLE_ACP2,scale.unit=F,ncp=5,graph=T,quali.sup=c(1),ind.sup=c(33:92))
Table2_ACP <- data.frame(res.pca_TABLE2$var$cor)
head(Table2_ACP)
Table2_ACP[order(-Table2_ACP$Dim.1),]
Table2_ACP[order(-Table2_ACP$Dim.2),]
Table2_ACP[order(-Table2_ACP$Dim.3),]
res.pca_TABLE2$eig
res.hcpc_TABLE2 = HCPC(res.pca_TABLE2)
res.hcpc_TABLE2$desc.var
plot.PCA(res.pca_TABLE2, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot.PCA(res.pca_TABLE2, axes=c(1,3), choix="var",lim.cos2.var = 0.1)
plot.PCA(res.pca_TABLE2, axes=c(1,2), choix="ind",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE2,axes=c(1,2),choice="map",draw.tree=FALSE)
plot(res.hcpc_TABLE2,axes=c(1,3),choice="map",draw.tree=FALSE)

fviz_cluster(res.hcpc_TABLE2,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             #palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                         "#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606"),
             ggtheme = theme_minimal(),
             main = "Factor map"
)

fviz_dend(res.hcpc_TABLE2, 
          cex = 0.7,                     # Taille du text
          #palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                      "#25FDE9", "#DF73FF", "#9E9E9E"),
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = c("#000000"),           # Couleur du rectangle
          labels_track_height = 500      # Augment l'espace pour le texte
)

#########################################################################################

################# PCA3
head(TABLE)
colnames(TABLE)
TABLE_ACP3 <- TABLE[,c(2,12,6:10,20,14:18,28,22:26,36,30:34)]
row.names(TABLE_ACP3) <- TABLE_ACP3$Country_area
head(TABLE_ACP3)
dim(TABLE_ACP3)

library(FactoMineR)
library(factoextra)
res.pca_TABLE3 = PCA(TABLE_ACP3,scale.unit=F,ncp=5,graph=T,quali.sup=c(1),ind.sup=c(33:92))
Table3_ACP <- data.frame(res.pca_TABLE3$var$cor)
head(Table3_ACP)
Table3_ACP[order(-Table3_ACP$Dim.1),]
Table3_ACP[order(-Table3_ACP$Dim.2),]
Table3_ACP[order(-Table3_ACP$Dim.3),]
res.pca_TABLE3$eig
res.hcpc_TABLE3 = HCPC(res.pca_TABLE3)
res.hcpc_TABLE3$desc.var
plot.PCA(res.pca_TABLE3, axes=c(1,2), choix="var",lim.cos2.var = 0.1)
plot.PCA(res.pca_TABLE3, axes=c(1,3), choix="var",lim.cos2.var = 0.1)
plot.PCA(res.pca_TABLE3, axes=c(1,2), choix="ind",lim.cos2.var = 0.1)
plot(res.hcpc_TABLE3,axes=c(1,2),choice="map",draw.tree=FALSE)
plot(res.hcpc_TABLE3,axes=c(1,3),choice="map",draw.tree=FALSE)

fviz_cluster(res.hcpc_TABLE3,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             #palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                         "#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606"),
             ggtheme = theme_minimal(),
             main = "Factor map"
)

fviz_dend(res.hcpc_TABLE2, 
          cex = 0.7,                     # Taille du text
          #palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          palette = c("#000000", "#FF0000", "#00FF00", "#0000FF",
                      "#25FDE9", "#DF73FF", "#9E9E9E"),
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = c("#000000"),           # Couleur du rectangle
          labels_track_height = 500      # Augment l'espace pour le texte
)

#########################################################################################

#########################ICI ICI ICI ICI ICI ############################################
####################### faire même graphique que précédemment avec LSF uniquement #######
####################### faire même graphique que précédemment avec les scientific estimates ########################
####################### faire même graphique en positionnant les valeurs de 2017 ???? ##############################
####################### ICI ICI ICI ICI ICI ########################################################################

##### 7 groupes ! #######

head(res.pca_TABLE3$ind.sup$coord)
dim(res.pca_TABLE3$ind.sup$coord)

head(res.pca_TABLE3$ind$coord)
a <- as.data.frame(res.pca_TABLE3$ind$coord)
a[order(a$Dim.1),]
a[order(a$Dim.2),]
rm(a)
cleanMem()
dim(res.pca_TABLE3$ind$coord)

head(res.hcpc_TABLE3$call$X)
dim(res.hcpc_TABLE3$call$X)

COORD <- data.frame(res.pca_TABLE3$ind.sup$coord)
COORD$clust <- "supp"
head(COORD)
row.names(COORD)

COORD_LSF <- COORD[c(1,3,5,7,9,11,12,14,15,16,22,27,32,35,37,38,39,40,42,44,46,48,50,52,54,56,57,58,60),]
COORD_LSF$clust <- "LSF"
row.names(COORD_LSF)

COORD_sc <- COORD[c(19,21,24,26,29,31,34,47),]
COORD_sc$clust <- "scientific"
row.names(COORD_sc)

inter <- data.frame(res.hcpc_TABLE3$call$X)
colnames(inter)
row.names(inter)
head(inter)

library(data.table)
inter <- data.table(inter)
COORD_clust <- inter[,.(Dim.1=mean(Dim.1,na.rm=TRUE),Dim.2=mean(Dim.2,na.rm=TRUE),Dim.3=mean(Dim.3,na.rm=TRUE),
                        Dim.4=mean(Dim.4,na.rm=TRUE),Dim.5=mean(Dim.5,na.rm=TRUE)),(clust)]
COORD_clust <- COORD_clust[,c("Dim.1","Dim.2","Dim.3","Dim.4","Dim.5","clust")]
COORD_clust <- data.frame(COORD_clust)
COORD_clust <- COORD_clust[order(COORD_clust$clust),]
row.names(COORD_clust)<-c("1","2","3","4","5","6","7")
row.names(COORD_clust)
COORD_clust

inter <- data.frame(res.hcpc_TABLE3$call$X)
colnames(inter)
head(inter)
row.names(inter)
COORD_sc_init <- inter[c(3,4,5,7,10,13,24,27),]
head(COORD_sc_init)
row.names(COORD_sc_init)

dim(COORD_LSF)
dim(COORD_clust)
colnames(COORD_LSF)
colnames(COORD_clust)
COORD_LSF <- rbind(COORD_LSF,COORD_clust)
dim(COORD_LSF)
colnames(COORD_LSF)
COORD_LSF <- COORD_LSF[order(COORD_LSF$clust),]
COORD_LSF

dim(COORD_sc)
dim(COORD_clust)
dim(COORD_sc_init)
colnames(COORD_sc)
colnames(COORD_clust)
colnames(COORD_sc_init)
COORD_sc <- rbind(COORD_sc,COORD_sc_init)
COORD_sc <- rbind(COORD_sc,COORD_clust)
dim(COORD_sc)
colnames(COORD_sc)
COORD_sc <- COORD_sc[order(COORD_sc$clust),]
COORD_sc

library(ggplot2)
ggpubr::show_point_shapes()
p <- ggpubr::ggscatter(COORD_LSF, "Dim.1", "Dim.2",
                       color="clust", size = 3, palette = c("#000000", "#FF0000", "#00FF00", "#0000FF","#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606"),  
                       shape = "clust",
                       point = "point" %in% c("point", "text"), 
                       label = rownames(COORD_LSF),
                       font.label = 12, repel = TRUE,
                       ellipse = FALSE, ellipse.type = "convex",
                       ellipse.alpha = 0.2, ellipse.level = 0.95,
                       mean.point=TRUE,
                       main = "Factor map", xlab = "Axis 1", ylab = "Axis 2",
                       ggtheme = theme_minimal()
)
p <- p + scale_shape_manual(values = c(1,2,3,4,5,6,7,8))
p

ggpubr::show_point_shapes()
p <- ggpubr::ggscatter(COORD_sc, "Dim.1", "Dim.2",
                       color="clust", size = 5, palette = c("#000000", "#FF0000", "#00FF00", "#0000FF","#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606"),  
                       shape = "clust",
                       point = "point" %in% c("point", "text"), 
                       label = rownames(COORD_sc),
                       font.label = 12, repel = TRUE,
                       ellipse = FALSE, ellipse.type = "convex",
                       ellipse.alpha = 0.2, ellipse.level = 0.95,
                       mean.point=FALSE,
                       main = "Factor map", xlab = "Axis 1", ylab = "Axis 2",
                       ggtheme = theme_minimal()
)
p <- p + scale_shape_manual(values = c(1,2,3,4,5,6,7,8))
p

# library(ggplot2)
# cbp1 <- c("#000000", "#FF0000", "#00FF00", "#0000FF",
#           "#25FDE9", "#DF73FF", "#9E9E9E", "#D1B606")
# ggplot(COORD) +
#   aes(x = Dim.1, y = Dim.2, colour = clust) +
#   scale_colour_manual(values=cbp1) +
#   geom_text(label=rownames(COORD)) +
#   theme_minimal() +
#   geom_point(size=1, shape=10,alpha=0.9) +
#   xlab("Axis 1") +
#   ylab("Axis 2") +
#   labs(size = "Cluster", colour = "Cluster")
#
# fviz_cluster <- function(object, data = NULL, choose.vars = NULL, stand = TRUE, 
#                          axes = c(1, 2),
#                          geom = c("point", "text"), repel = FALSE,
#                          show.clust.cent = TRUE,
#                          ellipse = TRUE, ellipse.type = "convex", ellipse.level = 0.95,
#                          ellipse.alpha = 0.2,
#                          shape = NULL, pointsize = 1.5, labelsize = 12,
#                          main = "Cluster plot",  xlab = NULL, ylab = NULL,
#                          outlier.color = "black", outlier.shape = 19,
#                          outlier.pointsize = pointsize, outlier.labelsize = labelsize,
#                          ggtheme = theme_grey(), ...)
# repel = TRUE
# colnames(ind) <- c("Dim.1", "Dim.2", "clust")
# p <- ggpubr::ggscatter(plot.data, "x", "y",
#                        color="cluster", shape = shape, size = pointsize,
#                        point = "point" %in% geom, 
#                        label = lab,
#                        font.label = labelsize, repel = repel,
#                        mean.point = show.clust.cent, 
#                        ellipse = ellipse, ellipse.type = ellipse.type,
#                        ellipse.alpha = ellipse.alpha, ellipse.level = ellipse.level,
#                        main = main, xlab = xlab, ylab = ylab,
#                        ggtheme = ggtheme, ...
# )



