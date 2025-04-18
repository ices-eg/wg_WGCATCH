library(openxlsx)
library(data.table)
library(ggplot2)
library(ggbreak)
library(ggnewscale)

rm(list=ls())
gc()

v<-9
cou.excl<-c("IRL")
areas<-c("27","37")

setwd("ToRb")
if(!dir.exists("results/plots")) dir.create("results/plots/", recursive = T)

data<-read.xlsx(xlsxFile="data/All_countries_WGCATCH 2021 ToR B peer_review_paper_data_2022_rev20250411.xlsx",
                sheet="WGCATCH SSF paper data")
data.source.char<-read.xlsx(xlsxFile="data/Data_source_characterization_table_v3.xlsx",
                            startRow = 3)

setwd("results")

data<-data.table(data)
data$Country_new<-substr(data$Country,1,3)
data[Country_new=="NO/",Country_new:="NOR"];.Last.updated
unique(data$Country_new)

table(data$`Vessel.length.ranges.(m)`,useNA = "always")
data[`Vessel.length.ranges.(m)` %in% c("[0-6[m","0-6[m"),`Vessel.length.ranges.(m)_new`:="0-6"];.Last.updated
data[`Vessel.length.ranges.(m)` %in% c("[6-8[m"),`Vessel.length.ranges.(m)_new`:="6-8"];.Last.updated
data[`Vessel.length.ranges.(m)` %in% c("[8-10[m"),`Vessel.length.ranges.(m)_new`:="8-10"];.Last.updated
data[`Vessel.length.ranges.(m)` %in% c("[10-12[m"),`Vessel.length.ranges.(m)_new`:="10-12"];.Last.updated
data[`Vessel.length.ranges.(m)` %in% c("[12-15[m"),`Vessel.length.ranges.(m)_new`:="12-15"];.Last.updated
data[`Vessel.length.ranges.(m)` %in% c(">=15",">=15m"),`Vessel.length.ranges.(m)_new`:="15 m and more"];.Last.updated
table(data$`Vessel.length.ranges.(m)_new`,useNA = "always")
# data$`Vessel.length.ranges.(m)_new`<-factor(data$`Vessel.length.ranges.(m)_new`,
#                                             levels = c("0-6","6-8","8-10","10-12",
#                                                        "12-15","15 m and more"))
data$`Vessel.length.ranges.(m)_new`<-factor(data$`Vessel.length.ranges.(m)_new`,
                                            levels = c("15 m and more","12-15","10-12",
                                                       "8-10","6-8","0-6"))

data.source.char<-data.table(data.source.char)
setnames(data.source.char, old=c("X1","X2"), new=c("Country.region","Vessel.length.range"))
data.source.char$Country<-substr(data.source.char$Country.region,1,3)
data.source.char$Region<-substr(data.source.char$Country.region,5,6)



data.source.char<-data.source.char[Region %in% areas, unique(.SD),
                                   .SDcols=c("Country","Region",
                                             "Vessel.length.range",
                                             "Data_source_characterization")]
data.source.char[Vessel.length.range==">=10m", Vessel.length.range.new:="10-15"];.Last.updated;
data.source.char[Vessel.length.range=="<10m", Vessel.length.range.new:="0-10"];.Last.updated;

table(data$Supra.Region_new, useNA = "always")

data.sum<-data[!Country_new %in% cou.excl & Supra.Region_new %in% areas,
               .(trips = sum(Number.of.fishing.trips.peformed.by.vessel.length.range, na.rm=T),
                 days.at.sea = sum(Number.of.Days.at.Sea.performed.by.vessel.length.range, na.rm=T),
                 landings.tons = sum(`Total.landings.in.weight.(tons).(all.species)`,na.rm=T),
                 landings.thousand.eur = sum(`Total.landings.in.value.(euros*1000).(all.species)`,na.rm=T),
                 active.vessels = sum(`Number.of.national."active".fishing.fleet.register.vessels.by.vessel.length.range`, na.rm = T)),
               by=.(Country=Country_new,
                    Year=Year.of.reference, Vessel.length=`Vessel.length.ranges.(m)_new`)]
# averages
data.sum[,":="(avg.landings.vessel = landings.tons/active.vessels,
               avg.landings.trip = landings.tons/trips,
               avg.days.vessel = days.at.sea/active.vessels,
               avg.days.trip = days.at.sea/trips,
               avg.value.vessel = landings.thousand.eur/active.vessels,
               avg.value.trip = landings.thousand.eur/trips,
               avg.landings.das = ifelse(days.at.sea>0,landings.tons/days.at.sea,NaN),
               avg.value.das = ifelse(days.at.sea>0,landings.thousand.eur/days.at.sea,NaN))]

palette.def.ssf <- c("12-15" = "#377EB8","10-12" = "#4DAF4A",
                 "8-10" = "#984EA3","6-8" = "#FF7F00","0-6" = "#FFFF33")
palette.def.all <- c("15 m and more" = "#E41A1C","12-15" = "#377EB8","10-12" = "#4DAF4A",
                     "8-10" = "#984EA3","6-8" = "#FF7F00","0-6" = "#FFFF33")


# Series 1 - SSF data
series1<-data.sum[Vessel.length!="15 m and more"]
series1[,":="(total.landings.tons=sum(landings.tons, na.rm=T),
              total.landings.thousand.eur=sum(landings.thousand.eur, na.rm = T),
              total.days.at.sea = sum(days.at.sea,na.rm=T),
              total.trips=sum(trips, na.rm = T),
              total.active.vessels=sum(active.vessels, na.rm = T)),
        by=.(Country,Year)]
# Series 2 - All data
series2<-data.sum[,.(landings.tons=sum(landings.tons, na.rm=T),
                     landings.thousand.eur=sum(landings.thousand.eur, na.rm = T),
                     days.at.sea = sum(days.at.sea,na.rm=T),
                     trips=sum(trips, na.rm = T),
                     active.vessels=sum(active.vessels, na.rm = T)),
                  by=.(Country,Year)]
# Series 3 - LSF data
series3<-data.sum[Vessel.length=="15 m and more"]
series3$Vessel.length <- as.character(series3$Vessel.length)
series3$Vessel.length <- factor(series3$Vessel.length)

# Landings weight with the y-axis break to show very high values
p <- ggplot()+
  geom_bar(data=series1,
           aes(x=reorder(Country, -total.landings.tons), y=landings.tons, fill=Vessel.length),
           stat="identity")+
  geom_point(data=series2,
             aes(x=Country,y=landings.tons,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings weight [t]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Landings weight in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]",
       tag="=")+
  scale_y_break(c(300000,395000),
                scales = 0.3)+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        plot.tag.position = c(0.095, 0.75))
p
ggsave(p,file=paste0("plots/1_Landings_weight_v",v,".png"),width = 10,height = 7)

# Landings weight with inset plot to show very low values (SSF only)
p <- ggplot()+
  geom_bar(data=series1[landings.tons > 0],
           aes(x=reorder(Country, -total.landings.tons), y=landings.tons, fill=Vessel.length),stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings weight [t]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Landings weight in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
inset.p <- ggplot()+
  geom_bar(data=series1[landings.tons > 0 & Country %in% 
                          series1[landings.tons>0,.(x=sum(landings.tons)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
           aes(x=reorder(Country, -total.landings.tons), y=landings.tons, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings weight [t]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x=NULL, 
       title = NULL,
       fill=NULL,
       subtitle = "10 lowest values")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        legend.position = "none")
inset.p
p<-p + annotation_custom(ggplotGrob(inset.p), xmin = 5, xmax = 17, ymin = 50000, ymax = 225000)
p
ggsave(p,file=paste0("plots/1_Landings_weight_inset_v",v,".png"),width = 10,height = 7)

# Landings weight percentage
p <- ggplot(data.sum, aes(x=Country, y=landings.tons, fill=Vessel.length))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Country", y="% of landings weight", 
       title = "Percentage of landings weight in 2019 in areas 27 and 37",
       fill="Vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
ggsave(p,file=paste0("plots/1_Landings_weight_perc_v",v,".png"),width = 10,height = 7)

# Landings value with the y-axis break to show very high values
p <- ggplot()+
  geom_bar(data=series1[landings.thousand.eur>0],
           aes(x=reorder(Country, -total.landings.thousand.eur), y=landings.thousand.eur, fill=Vessel.length),
           stat="identity")+
  geom_point(data=series2[landings.thousand.eur>0],
             aes(x=Country,y=landings.thousand.eur,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings value [1000 EUR]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Landings value in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]",
       tag="=")+
  scale_y_break(c(450000,600000), 
                scales = 0.4)+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        plot.tag.position = c(0.095, 0.705))
p
ggsave(p,file=paste0("plots/1_Landings_value_v",v,".png"),width = 10,height = 7)

# Landings value with inset plot to show very low values (SSF only)
p <- ggplot()+
  geom_bar(data=series1[landings.thousand.eur > 0],
           aes(x=reorder(Country, -total.landings.thousand.eur), y=landings.thousand.eur, fill=Vessel.length),stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings value [1000 EUR]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Landings value in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
inset.p <- ggplot()+
  geom_bar(data=series1[landings.thousand.eur > 0 & Country %in% 
                          series1[landings.thousand.eur>0,.(x=sum(landings.thousand.eur)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
           aes(x=reorder(Country, -total.landings.thousand.eur), y=landings.thousand.eur, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings value [1000 EUR]")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x=NULL, 
       title = NULL,
       fill=NULL,
       subtitle = "10 lowest values")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        legend.position = "none")
inset.p
p<-p + annotation_custom(ggplotGrob(inset.p), xmin = 6, xmax = 17, ymin = 150000, ymax = 450000)
p
ggsave(p,file=paste0("plots/1_Landings_value_inset_v",v,".png"),width = 10,height = 7)

# Landings value percentage
p <- ggplot(data.sum[landings.thousand.eur>0], 
            aes(x=Country, y=landings.thousand.eur, fill=Vessel.length))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Country", y="% of landings value", 
       title = "Percentage of landings value in 2019 in areas 27 and 37",
       fill="Vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
ggsave(p,file=paste0("plots/1_Landings_value_perc_v",v,".png"),width = 10,height = 7)


# Days at sea
p <- ggplot()+
  geom_bar(data=series1[days.at.sea > 0],
           aes(x=reorder(Country, -total.days.at.sea), y=days.at.sea, fill=Vessel.length),stat="identity")+
  geom_point(data=series2[days.at.sea>0],
             aes(x=Country,y=days.at.sea,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Days at sea")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Days at sea in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
inset.p <- ggplot()+
  geom_bar(data=series1[days.at.sea > 0 & Country %in% 
                          series1[days.at.sea>0,.(x=sum(days.at.sea)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
           aes(x=reorder(Country, -total.days.at.sea), y=days.at.sea, fill=Vessel.length),
           stat="identity")+
  geom_point(data=series2[days.at.sea>0 & Country %in%
                            series1[days.at.sea>0,.(x=sum(days.at.sea)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
             aes(x=Country,y=days.at.sea,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Days at sea")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x=NULL, 
       title = NULL,
       fill=NULL,
       subtitle = "10 lowest values")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        legend.position = "none")
inset.p
p<-p + annotation_custom(ggplotGrob(inset.p), xmin = 5, xmax = 16.5, ymin = 500000, ymax = 1800000)
p
ggsave(p,file=paste0("plots/1_Days_at_sea_v",v,".png"),width = 10,height = 7)


# Days at sea percentage
p <- ggplot(data.sum[days.at.sea>0], 
            aes(x=Country, y=days.at.sea, fill=Vessel.length))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Country", y="% of days at sea", 
       title = "Percentage of days at sea in 2019 in areas 27 and 37",
       fill="Vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
ggsave(p,file=paste0("plots/1_Days_at_sea_perc_v",v,".png"),width = 10,height = 7)

# Number of trips
p <- ggplot()+
  geom_bar(data=series1[trips > 0],
           aes(x=reorder(Country, -total.trips), y=trips, fill=Vessel.length),stat="identity")+
  geom_point(data=series2[trips>0],
             aes(x=Country,y=trips,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of trips")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Number of trips in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p

inset.p <- ggplot()+
  geom_bar(data=series1[trips > 0 & Country %in% 
                          series1[trips>0,.(x=sum(trips)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
           aes(x=reorder(Country, -total.trips), y=trips, fill=Vessel.length),
           stat="identity")+
  geom_point(data=series2[trips>0 & Country %in%
                            series1[trips>0,.(x=sum(trips)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
             aes(x=Country,y=trips,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of trips")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x=NULL, 
       title = NULL,
       fill=NULL,
       subtitle = "10 lowest values")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        legend.position = "none")
inset.p
p<-p + annotation_custom(ggplotGrob(inset.p), xmin = 5, xmax = 16.5, ymin = 500000, ymax = 1800000)
p
ggsave(p,file=paste0("plots/1_Trips_v",v,".png"),width = 10,height = 7)

# Number of trips percentage
p <- ggplot(data.sum[trips > 0], aes(x=Country, y=trips, fill=Vessel.length))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Country", y="% of number of trips", 
       title = "Percentage of number of trips in 2019 in areas 27 and 37",
       fill="Vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))

ggsave(p,file=paste0("plots/1_Trips_perc_v",v,".png"),width = 10,height = 7)

# Number of active vessels
p <- ggplot()+
  geom_bar(data=series1[active.vessels > 0],
           aes(x=reorder(Country, -total.active.vessels), y=active.vessels, fill=Vessel.length),stat="identity")+
  geom_point(data=series2[active.vessels>0],
             aes(x=Country,y=active.vessels,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of active vessels")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x="Country", 
       title = "Number of active vessels in 2019 in areas 27 and 37",
       fill="SSF vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
inset.p <- ggplot()+
  geom_bar(data=series1[active.vessels > 0 & Country %in% 
                          series1[active.vessels>0,.(x=sum(active.vessels)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
           aes(x=reorder(Country, -total.active.vessels), y=active.vessels, fill=Vessel.length),
           stat="identity")+
  geom_point(data=series2[active.vessels>0 & Country %in%
                            series1[active.vessels>0,.(x=sum(active.vessels)),by=.(Country)][order(-x)][(.N-9):.N,Country]],
             aes(x=Country,y=active.vessels,color="black"),size=10,shape=95)+
  scale_fill_manual(values = palette.def.ssf)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of active vessel")+
  scale_color_identity(guide = "legend", labels=NULL, name="Entire fleet")+
  labs(x=NULL, 
       title = NULL,
       fill=NULL,
       subtitle = "10 lowest values")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"),
        legend.position = "none")
inset.p
p<-p + annotation_custom(ggplotGrob(inset.p), xmin = 6, xmax = 17, ymin = 4000, ymax = 11500)
p
ggsave(p,file=paste0("plots/1_Active_vessels_v",v,".png"),width = 10,height = 7)

# Number of active vessels percentage
p <- ggplot(data.sum[active.vessels > 0], aes(x=Country, y=active.vessels, fill=Vessel.length))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Country", y="% of number of active vessels", 
       title = "Percentage of number of active vessels in 2019 in areas 27 and 37",
       fill="Vessel length [m]")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))

ggsave(p,file=paste0("plots/1_Active_vessels_perc_v",v,".png"),width = 10,height = 7)


# Data_source_characterization_table
data.sum.char<-data[!Country_new %in% cou.excl & Supra.Region_new %in% areas &
                      `Vessel.length.ranges.(m)_new` != "15 m and more",
                    .(trips = sum(Number.of.fishing.trips.peformed.by.vessel.length.range, na.rm=T),
                      days.at.sea = sum(Number.of.Days.at.Sea.performed.by.vessel.length.range, na.rm=T),
                      landings.tons = sum(`Total.landings.in.weight.(tons).(all.species)`,na.rm=T),
                      landings.thousand.eur = sum(`Total.landings.in.value.(euros*1000).(all.species)`,na.rm=T)),
                    by=.(Country=Country_new,
                         Year=Year.of.reference, Vessel.length=`Vessel.length.ranges.(m)_new`,
                         Region=Supra.Region_new)]
table(data.sum.char$Vessel.length)
data.sum.char[Vessel.length %in% c("0-6","6-8","8-10"),Vessel.length.range.new:="0-10"];.Last.updated
data.sum.char[Vessel.length %in% c("10-12","12-15"),Vessel.length.range.new:="10-15"];.Last.updated
data.sum.char<-data.sum.char[,
                    .(trips = sum(trips, na.rm=T),
                      days.at.sea = sum(days.at.sea, na.rm=T),
                      landings.tons = sum(landings.tons,na.rm=T),
                      landings.thousand.eur = sum(landings.thousand.eur,na.rm=T)),
                    by=.(Country,Year, Vessel.length, Vessel.length.range.new,Region)]
data.sum.char<-merge(data.sum.char, data.source.char,
                     all.x = T, by=c("Country","Region","Vessel.length.range.new"))
data.sum.char<-data.sum.char[,.(trips = sum(trips, na.rm=T),
                                days.at.sea = sum(days.at.sea, na.rm=T),
                                landings.tons = sum(landings.tons,na.rm=T),
                                landings.thousand.eur = sum(landings.thousand.eur,na.rm=T)),
                             by=.(Vessel.length, Data_source_characterization)]
data.sum.char$Vessel.length<-factor(data.sum.char$Vessel.length,
                                            levels = c("0-6","6-8","8-10","10-12",
                                                       "12-15"))
# data.sum.char$Data_source_characterization<-factor(data.sum.char$Data_source_characterization,
#                                     levels = c("C","C+","B","A","A+"))
data.sum.char$Data_source_characterization<-factor(data.sum.char$Data_source_characterization,
                                                   levels = c("A+","A","B","C+","C","X"))
palette.def.char <- c("A+" = "#4DAF4A","A" = "#B3DE69",
                     "B" = "#FFFF33","C+" = "#FF7F00","C" = "#E41A1C",
                     "X" = "#B3B3B3")

# Data_source_characterization_table landings weight
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=landings.tons, fill=Data_source_characterization),
           stat="identity")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings weight [t]")+
  labs(x="Vessels length range", 
       title = "Landings weight in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_Landings_weight_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings weight percentage
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=landings.tons, fill=Data_source_characterization),
           stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of landings weight")+
  labs(x="Vessels length range", 
       title = "Percentage of landings weight in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_Landings_weight_perc_v",v,".png"),width = 10,height = 7)
# Data_source_characterization_table landings value
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=landings.thousand.eur, fill=Data_source_characterization),
           stat="identity")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings value [1000 EUR]")+
  labs(x="Vessels length range", 
       title = "Landings value in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_Landings_value_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings value percentage
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=landings.thousand.eur, fill=Data_source_characterization),
           stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of landings value")+
  labs(x="Vessels length range", 
       title = "Percentage of landings value in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_Landings_value_perc_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table days at sea
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=days.at.sea, fill=Data_source_characterization),
           stat="identity")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Days at sea")+
  labs(x="Vessels length range", 
       title = "Days at sea in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_days_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table days at sea percentage
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=days.at.sea, fill=Data_source_characterization),
           stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of days at sea")+
  labs(x="Vessels length range", 
       title = "Percentage of days at sea in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_days_perc_v",v,".png"),width = 10,height = 7)


# Data_source_characterization_table trips
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=trips, fill=Data_source_characterization),
           stat="identity")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of trips")+
  labs(x="Vessels length range", 
       title = "Number of trips in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_trips_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table trips percentage
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Vessel.length, y=trips, fill=Data_source_characterization),
           stat="identity",position="fill")+
  scale_fill_manual(values = palette.def.char)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of number of trips")+
  labs(x="Vessels length range", 
       title = "Percentage of number of trips in 2019 in areas 27 and 37",
       fill="Data source grades")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/2_Source_char_trips_perc_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings weight re-arranged
data.sum.char$Vessel.length<-factor(data.sum.char$Vessel.length,
                                    levels = c("12-15","10-12","8-10","6-8","0-6"))
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=landings.tons, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings weight [t]")+
  labs(x="Data source grade", 
       title = "Landings weight in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_landings_weight_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings weight percentage re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=landings.tons, fill=Vessel.length),
           stat="identity", position="fill")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of landings weight")+
  labs(x="Data source grade", 
       title = "Percentage of landings weight in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_landings_weight_perc_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings value re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=landings.thousand.eur, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Landings value [1000 EUR]")+
  labs(x="Data source grade", 
       title = "Landings value in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_landings_value_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table landings value percent re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=landings.thousand.eur, fill=Vessel.length),
           stat="identity", position="fill")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of landings value")+
  labs(x="Data source grade", 
       title = "Percentage of landings value in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_landings_value_perc_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table days at sea re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=days.at.sea, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Days at sea")+
  labs(x="Data source grade", 
       title = "Days at sea in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_Days_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table days at sea percentage re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=days.at.sea, fill=Vessel.length),
           stat="identity", position="fill")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of days at sea")+
  labs(x="Data source grade", 
       title = "Percentage of days at sea in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_Days_perc_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table trips re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=trips, fill=Vessel.length),
           stat="identity")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Number of trips")+
  labs(x="Data source grade", 
       title = "Number of trips in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_Trips_v",v,".png"),width = 10,height = 7)

# Data_source_characterization_table trips percentage re-arranged
p <- ggplot()+
  geom_bar(data=data.sum.char[!is.na(Data_source_characterization)],
           aes(x=Data_source_characterization, y=trips, fill=Vessel.length),
           stat="identity", position="fill")+
  scale_fill_manual(values = palette.def.ssf)+
  #scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::percent,name="% of number of trips")+
  labs(x="Data source grade", 
       title = "Percentage of number of trips in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/3_Source_char_Trips_perc_v",v,".png"),width = 10,height = 7)


# Plots of averages

# Average landings per vessel
plot.factor <- max(series1$avg.landings.vessel, na.rm = T) / max(series3$avg.landings.vessel, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.landings.vessel>0],
           aes(x=Country, y=avg.landings.vessel, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  geom_point(data=series3, aes(x=Country, y=(avg.landings.vessel * plot.factor), shape="15 m and more"), size=2.5)+
  geom_segment(data=series3, aes(x=Country, xend=Country, y=0, yend=avg.landings.vessel * plot.factor))+
  scale_colour_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings/vessel SSF [t]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings/vessel LSF [t]"))+
  labs(x="Country", 
       title = "Average landings weight per vessel in 2019 in areas 27 and 37",
       colour="Small scale fleet\nvessels length range",
       shape="Large scale fleet")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_weight_vessel_v",v,".png"),width = 10,height = 7)

# Average landings per vessel - revised
plot.factor <- max(series1$avg.landings.vessel, na.rm = T) / max(series3$avg.landings.vessel, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.landings.vessel>0],
             aes(x=Country, y=avg.landings.vessel, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range", values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3, aes(x=Country, 
                               y=(avg.landings.vessel * plot.factor), 
                               colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings/vessel SSF [t]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings/vessel LSF [t]"))+
  labs(x="Country", 
       title = "Average landings weight per vessel in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_weight_vessel_v",v,"_revised.png"),width = 10,height = 7)

# Average landings per trip
plot.factor <- max(series1$avg.landings.trip, na.rm = T) / max(series3$avg.landings.trip, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.landings.trip>0],
             aes(x=Country, y=avg.landings.trip, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  geom_point(data=series3, aes(x=Country, y=(avg.landings.trip * plot.factor), shape="15 m and more"), size=2.5)+
  geom_segment(data=series3, aes(x=Country, xend=Country, y=0, yend=avg.landings.trip * plot.factor))+
  scale_colour_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings/trip SSF [t]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings/trip LSF [t]"))+
  labs(x="Country", 
       title = "Average landings weight per trip in 2019 in areas 27 and 37",
       colour="Small scale fleet\nvessels length range",
       shape="Large scale fleet")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_weight_trips_v",v,".png"),width = 10,height = 7)

# Average landings per trip - revised
plot.factor <- max(series1$avg.landings.trip, na.rm = T) / max(series3$avg.landings.trip, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.landings.trip>0],
             aes(x=Country, y=avg.landings.trip, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range", values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3, aes(x=Country, 
                               y=(avg.landings.trip * plot.factor), 
                               colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings/trip SSF [t]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings/trip LSF [t]"))+
  labs(x="Country", 
       title = "Average landings weight per trip in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_weight_trips_v",v,"_revised.png"),width = 10,height = 7)

# Average effort per vessel
p <- ggplot()+
  geom_bar(data=data.sum[avg.days.vessel>0],
             aes(x=Country, y=avg.days.vessel, fill=Vessel.length), size=2.5, 
           position = position_dodge(preserve = "single"),
           stat="identity", width = 0.8)+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. days at sea/vessel")+
  labs(x="Country", 
       title = "Average days at sea per vessel in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_days_vessel_v",v,".png"),width = 10,height = 7)


# Average effort per trip
p <- ggplot()+
  geom_bar(data=data.sum[avg.days.trip>0],
           aes(x=Country, y=avg.days.trip, fill=Vessel.length), size=2.5, 
           position = position_dodge(preserve = "single"),
           stat="identity", width = 0.8)+
  scale_fill_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. days at sea/trip")+
  labs(x="Country", 
       title = "Average days at sea per trip in 2019 in areas 27 and 37",
       fill="Vessels length range")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_days_trip_v",v,".png"),width = 10,height = 7)

# Average landings value per vessel
plot.factor <- max(series1$avg.value.vessel, na.rm = T) / max(series3$avg.value.vessel, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.value.vessel>0],
             aes(x=Country, y=avg.value.vessel, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  geom_point(data=series3, aes(x=Country, y=(avg.value.vessel * plot.factor), shape="15 m and more"), size=2.5)+
  geom_segment(data=series3, aes(x=Country, xend=Country, y=0, yend=avg.value.vessel * plot.factor))+
  scale_colour_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings value/vessel SSF [1000 EUR]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings value/vessel LSF [1000 EUR]"))+
  labs(x="Country", 
       title = "Average landings value per vessel in 2019 in areas 27 and 37",
       colour="Small scale fleet\nvessels length range",
       shape="Large scale fleet")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_value_vessel_v",v,".png"),width = 10,height = 7)

# Average landings value per vessel - revised
plot.factor <- max(series1$avg.value.vessel, na.rm = T) / max(series3$avg.value.vessel, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.value.vessel>0],
             aes(x=Country, y=avg.value.vessel, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range", values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3, aes(x=Country, 
                               y=(avg.value.vessel * plot.factor), 
                               colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings value/vessel SSF [1000 EUR]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings value/vessel LSF [1000 EUR]"))+
  labs(x="Country", 
       title = "Average landings value per vessel in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_value_vessel_v",v,"_revised.png"),width = 10,height = 7)

# Average landings value per trip
plot.factor <- max(series1$avg.value.trip, na.rm = T) / max(series3$avg.value.trip, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.value.trip>0],
             aes(x=Country, y=avg.value.trip, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  geom_point(data=series3, aes(x=Country, y=(avg.value.trip * plot.factor), shape="15 m and more"), size=2.5)+
  geom_segment(data=series3, aes(x=Country, xend=Country, y=0, yend=avg.value.trip * plot.factor))+
  scale_colour_manual(values = palette.def.all)+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings value/trip SSF [1000 EUR]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings value/trip LSF [1000 EUR]"))+
  labs(x="Country", 
       title = "Average landings value per trip in 2019 in areas 27 and 37",
       colour="Small scale fleet\nvessels length range",
       shape="Large scale fleet")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_value_trip_v",v,".png"),width = 10,height = 7)

# Average landings value per trip - revised
plot.factor <- max(series1$avg.value.trip, na.rm = T) / max(series3$avg.value.trip, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.value.trip>0],
             aes(x=Country, y=avg.value.trip, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range",values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3, aes(x=Country, 
                               y=(avg.value.trip * plot.factor), 
                               colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings value/trip SSF [1000 EUR]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings value/trip LSF [1000 EUR]"))+
  labs(x="Country", 
       title = "Average landings value per trip in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_value_trip_v",v,"_revised.png"),width = 10,height = 7)

# Average landings per day at sea
plot.factor <- max(series1$avg.landings.das, na.rm = T) / max(series3$avg.landings.das, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.landings.das>0 & days.at.sea>0],
             aes(x=Country, y=avg.landings.das, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range", values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3[avg.landings.das>0 & days.at.sea>0], aes(x=Country, 
                               y=(avg.landings.das * plot.factor), 
                               colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings/day at sea SSF [t]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings/day at sea LSF [t]"))+
  labs(x="Country", 
       title = "Average landings weight per day at sea in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_weight_das_v",v,".png"),width = 10,height = 7)

# Average value per day at sea
plot.factor <- max(series1$avg.value.das, na.rm = T) / max(series3$avg.value.das, na.rm = T)
p <- ggplot()+
  geom_point(data=series1[avg.value.das>0 & days.at.sea>0],
             aes(x=Country, y=avg.value.das, colour=Vessel.length), size=2.5, position = position_dodge(width=0.5))+
  scale_colour_manual("Small scale fleet\nvessels length range", values = palette.def.all)+
  new_scale_colour()+
  geom_point(data=series3[avg.value.das>0 & days.at.sea>0], aes(x=Country, 
                                                                   y=(avg.value.das * plot.factor), 
                                                                   colour=Vessel.length), 
             shape=22, fill="#E41A1C", size=3.5, alpha=0.5)+
  scale_colour_manual("Large scale fleet", values = "black")+
  scale_x_discrete(guide = guide_axis(angle=45))+
  scale_y_continuous(labels = scales::number,name="Avg. landings value/day at sea SSF [1000 EUR]",
                     sec.axis = sec_axis(~./plot.factor, name = "Avg. landings value/day at sea LSF [1000 EUR]"))+
  labs(x="Country", 
       title = "Average landings value per day at sea in 2019 in areas 27 and 37")+
  theme(axis.line.y.left=element_line(colour="black"),
        axis.line.y.right = element_line(colour="black"),
        axis.ticks.y.right = element_line(colour="black"),
        axis.line.x.bottom = element_line(colour="black"))
p
ggsave(p,file=paste0("plots/4_avg_landings_value_das_v",v,".png"),width = 10,height = 7)
#brewer.pal(n=8,"Set2")
#display.brewer.pal(n=8,"Set2")
