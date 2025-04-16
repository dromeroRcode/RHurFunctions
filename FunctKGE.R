KGE2 <- function(x,y){
  cc <- cor(x,y)
  aa <- (sd(y)/mean(y))/(sd(x)/mean(x))
  bb<-mean(y)/mean(x)
  KGE<-1-(sqrt((cc-1)^2+(aa-1)^2+(bb-1)^2))
  KGE}
                   
