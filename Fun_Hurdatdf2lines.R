Hurdatdf2lines<-function(c00){
  library(tidyr)
  library(sp)
  ids<-unique(c00$IDhur)
  h1<-c00[which(c00$IDhur==ids[1]),]
  h1$NT<-seq(1,nrow(h1),1)
  h1$LATini<-NA
  h1$LONGini<-NA
  h1$LATfin<-NA
  h1$LONGfin<-NA
  h1$DTn<-as.numeric(h1$DateTime)
  h1$Tant<-NA
  h1$Tpost<-NA
  for (j in 2:(nrow(h1)-1)){
    h1$LATini[j]<-h1$Lat[j-1]
    h1$LONGini[j]<-h1$Long[j-1]
    h1$LATfin[j]<-h1$Lat[j+1]
    h1$LONGfin[j]<-h1$Long[j+1]
    h1$Tant[j]<-h1$DTn[j-1]
    h1$Tpost[j]<-h1$DTn[j+1]}
  
  for (i in 2:length(ids)){
    hi<-c00[which(c00$IDhur==ids[i]),]
    if(nrow(hi)>2){
      hi$NT<-seq(1,nrow(hi),1)
      hi$LATini<-NA
      hi$LONGini<-NA
      hi$LATfin<-NA
      hi$LONGfin<-NA
      hi$DTn<-as.numeric(hi$DateTime)
      hi$Tant<-NA
      hi$Tpost<-NA
      for (j in 2:(nrow(hi)-1)){
        hi$LATini[j]<-hi$Lat[j-1]
        hi$LONGini[j]<-hi$Long[j-1]
        hi$LATfin[j]<-hi$Lat[j+1]
        hi$LONGfin[j]<-hi$Long[j+1]
        hi$Tant[j]<-hi$DTn[j-1]
        hi$Tpost[j]<-hi$DTn[j+1]}
      h1<-rbind(h1,hi)}
  }

  h1$LATiniR<-(h1$LATini+h1$Lat)/2
  h1$LONGiniR<-(h1$LONGini+h1$Long)/2
  h1$LATfinR<-(h1$LATfin+h1$Lat)/2
  h1$LONGfinR<-(h1$LONGfin+h1$Long)/2
  h1$Resid<-((h1$DTn-h1$Tant)/2+(h1$Tpost-h1$DTn)/2)/3600
  
  hur<-h1 %>% drop_na()
  hur$ID<-seq(1,nrow(hur),1)
  hur_lines <- apply(hur,1,function(x){
    points <- data.frame(lng=as.numeric(c(x["LONGiniR"],x["Long"],x["LONGfinR"])),
                         lat=as.numeric(c(x["LATiniR"],x["Lat"],x["LATfinR"])),stringsAsFactors = F)
    coordinates(points) <- c("lng","lat")
    Lines(Line(points),ID=as.numeric(x["ID"]))})
  row.names(hur) <- hur$ID
  hur_lines <- SpatialLinesDataFrame(SpatialLines(hur_lines),hur[,-c(9:20)])
  
  return(hur_lines)
}
