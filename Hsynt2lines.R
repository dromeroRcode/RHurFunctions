Hsynt2lines<-function(x,wind,output){
  library(tidyr)
  library(sp)
h1<-read.delim(x, header=FALSE)[,-c(6,7)]
names(h1)<-c("HUR","YEAR","MONTH","DAY","HOUR","LAT","LONG","WIND","RAD","PRES","PRESA")
h1$NT<-seq(1,nrow(h1),1)
h1$LATini<-NA
h1$LONGini<-NA
h1$LATfin<-NA
h1$LONGfin<-NA
h1$Date<- as.Date(paste(h1$YEAR,h1$MONTH,h1$DAY,sep="-"),format = "%Y-%m-%d")
h1$DateTime<-strptime(paste(h1$Date,paste(h1$HOUR,":00",sep="")),"%Y-%m-%d %H:%M",tz = "UTC")
h1$DTn<-as.numeric(h1$DateTime)
h1$Tant<-NA
h1$Tpost<-NA
h11<-h1[h1$HUR==unique(h1$HUR)[1],]
for (j in 2:(nrow(h11)-1)){
  h11$LATini[j]<-h11$LAT[j-1]
  h11$LONGini[j]<-h11$LONG[j-1]
  h11$LATfin[j]<-h11$LAT[j+1]
  h11$LONGfin[j]<-h11$LONG[j+1]
  h11$Tant[j]<-h11$DTn[j-1]
  h11$Tpost[j]<-h11$DTn[j+1]
}
c0<-h11
for(i in 2:length(unique(h1$HUR))){
  h1i<-h1[h1$HUR==unique(h1$HUR)[i],]
  for (j in 2:(nrow(h1i)-1)){
    h1i$LATini[j]<-h1i$LAT[j-1]
    h1i$LONGini[j]<-h1i$LONG[j-1]
    h1i$LATfin[j]<-h1i$LAT[j+1]
    h1i$LONGfin[j]<-h1i$LONG[j+1]
    h1i$Tant[j]<-h1i$DTn[j-1]
    h1i$Tpost[j]<-h1i$DTn[j+1]
  }
  c0<-rbind(c0,h1i)
}
c0$LATiniR<-(c0$LATini+c0$LAT)/2
c0$LONGiniR<-(c0$LONGini+c0$LONG)/2
c0$LATfinR<-(c0$LATfin+c0$LAT)/2
c0$LONGfinR<-(c0$LONGfin+c0$LONG)/2
c0$Resid<-((c0$DTn-c0$Tant)/2+(c0$Tpost-c0$DTn)/2)/3600
c0$WINDkt<-c0$WIND*1.9438444924574
hur<-c0 %>% drop_na()
hur$ID<-seq(1,nrow(hur),1)
hur_lines <- apply(hur,1,function(x){
  points <- data.frame(lng=as.numeric(c(x["LONGiniR"],x["LONG"],x["LONGfinR"])),
                       lat=as.numeric(c(x["LATiniR"],x["LAT"],x["LATfinR"])),stringsAsFactors = F)
  coordinates(points) <- c("lng","lat")
  Lines(Line(points),ID=as.numeric(x["ID"]))})
row.names(hur) <- hur$ID
hur<-hur[,-c(10:16,19:25)]
hur_lines <- SpatialLinesDataFrame(SpatialLines(hur_lines),hur)

lineas<-st_as_sf(hur_lines[hur_lines$WINDkt>=wind,])
lineas$LenOrig<-st_length(lineas)
lineas$Veltrans<-as.numeric(lineas$LenOrig)/1000/lineas$Resid
st_write(lineas,output)
return(output)
}