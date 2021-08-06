ConvHurdat<-function(a){
  a$IDhur<-NA
  a$Name<-NA
  
  for(i in 2:nrow(a)){
    if(!is.na(a[i,7]) & is.na(a[i-1,7])){
      a[i,23]=paste(a[i-1,1])
      a[i,24]=paste(a[i-1,2])}
    if(!is.na(a[i,7]) & !is.na(a[i-1,7])){
      a[i,23]=paste(a[i-1,23])
      a[i,24]=paste(a[i-1,24])}
  }
  a<-a[!is.na(a$V7),]
  a$Date<-as.Date(a$V1,format = "%Y%m%d")
  a$DateTime<-strptime(paste(a$V1,a$V2),"%Y%m%d %H%M",tz = "UTC")
  a$Year<-year(a$Date)
  a$hemi<-str_sub(a$V6,-1)
  a$Long<-as.numeric(paste(str_remove_all(a$V6, "[ WE]")))
  a$Long[a$hemi=="W"]<-a$Long[a$hemi=="W"]*(-1)
  a$Lat<-as.numeric(paste(str_remove_all(a$V5, "[ N]")))
  a$Wind<-a$V7
  
  b<-a[,-c(1:21,25,28)]
  return(b)}
