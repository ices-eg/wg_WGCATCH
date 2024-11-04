# ==================================
# Summaries of DLS data
# Nuno, Sven, Kirsten, Karolina @ WGCATCH 2018-2019
# ==================================

rm(list=ls())

# loads packages
    #...

# specify project you are working on
    project <- "WGBFAS_DLS_2018"
    #input_file <- "WGBFAS_flatfish_2019"
    #input_file <- "WGNSSK_sol274_2019"
    
 # directory structure   
    dir_inputs <- "001_Inputs/"
    dir_inputs_project <- paste(dir_inputs, project,"/", sep="")
    dir_outputs <- "002_Summaries/"
    dir_outputs_project <- paste(dir_outputs, project, "/", sep="")
    dir.create(dir_outputs_project, recursive=TRUE)

   
# loads clean/prepared data
    load(paste(dir_inputs_project, "001_data_prepared_",project,".Rdata", sep=""))

    df2<-df1[!is.na(df1$DLS),]

# graph 1
    
    df2<-df2[order(as.character(df2$stockCode)),]
    
    cores<-rainbow(length(unique(df2$stockCode)))
   
    windows(10,7)
    par(mfrow=c(2,2), oma=c(1,2,1,1))
    barplot(b<-table(df2$stockCode, df2$totTripsObsSppClass), las=2, col=cores, main = "No. trips obs with spp observed", ylab="frequency")
    #legend(x=12, y=max(apply(b,2,sum)), legend = unique(df2$stockCode), col = cores, pch=19, pt.cex=1.5)
    par(new=F)
    barplot(b<-table(df2$stockCode, df2$totTripsObsSppLtClass), las=2, col=cores, main = "No. trips obs with spp measured", ylab="frequency")
    legend(x=12, y=max(apply(b,2,sum))+1, legend = unique(df2$stockCode), col = cores, pch=19, pt.cex=1.2)
    par(new=F)
    barplot(b<-table(df2$stockCode, df2$totFishMeasClass), las=2, col=cores, main = "No. indiv measured", ylab="frequency")
    #legend(x=15, y=max(apply(b,2,sum)), legend = unique(df2$stockCode), col = cores, pch=19, pt.cex=1.5)
    savePlot(paste("002_Summaries/",project,"/", project,"_graph1", sep=""),type="png")
    
    
# graph 2

    # selected stocks
    table(df2$stockCode,df2$totTripsObsSppLtClass)

    windows(10,5)
    par(mfrow=c(1,2))
    plot(totVol/1000~totTripsObsSppLt, data = df2, ylab = "tonnes", xlab = "No. trips obs with spp observed", pch=19, cex.lab=0.8, cex.axis=0.8); #abline(v=3, lty=2, col="red")
    for (i in 1:length(unique(df2$stockCode)))
    {
    points(totVol/1000~totTripsObsSppLt, data = df2[df2$stockCode== unique(df2$stockCode)[i],], col=cores[i], pch=19) 
    }
    legend("topright", legend = unique(df2$stockCode), col = cores, pch=19, pt.cex=1, cex=0.8)

    plot(totVol/1000~totFishMeas, data = df2, ylab = "tonnes", xlab = "No. fish measured", pch=19, cex.lab=0.7, cex.axis=0.7); #abline(v=30, lty=2, col="red")
    for (i in 1:length(unique(df2$stockCode)))
    {
    points(totVol/1000~totFishMeas, data = df2[df2$stockCode== unique(df2$stockCode)[i],], col=cores[i], pch=19) 
    }
    legend("topright", legend = unique(df2$stockCode), col = cores, pch=19, pt.cex=1, cex=0.8)
   savePlot(paste("002_Summaries/",project,"/", project,"_graph2", sep=""),type="png")
 


