AjustNormGraph<-function (x,xlab,ylab,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
  if (type=="min"){aa="topleft"}else{aa="bottomleft"}
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  a1$Famp<-(a1$Num-0.3)/(mn+0.4)
  a1$Tretor<-1/a1$Famp
  a1$Uexp<-qnorm(a1$Famp)
  mA<-mean(a1$Variable)
  deA<-sd(a1$Variable)
  a1$Qteor<-qnorm(a1$Famp,mA,deA)
  a1$Fteor<-pnorm(a1$Variable,mA,deA)
  a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
  KomolgorovTest<-round(max(a1$Komolgorov),3)
  a1$Sxp<-sqrt(((deA^2)/mn)*(1+(a1$Uexp^2/2)))
  a1$Int1<-a1$Qteor-(2*a1$Sxp)
  a1$Int2<-a1$Qteor+(2*a1$Sxp)
  F<-c(0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.995,1-(1/max(a1$Tretor)*0.9))
  if (type=="min"){T<-1/F} else {T<-1/(1-F)}
  b<-data.frame(cbind(F,T))
  b$Uf<-qnorm(b$F)
  b$Q<-mA+b$Uf*deA
  b$Sxp<-sqrt(((deA^2)/mn)*(1+(b$Uf^2/2)))
  b$Int1<-b$Q-(2*b$Sxp)
  b$Int2<-b$Q+(2*b$Sxp)

  r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
  plot(a1$Variable~a1$Tretor,ylim=c(0,max(a1$Variable)*1.05),pch=3,xlim=c(1,max(a1$Tretor)+50),xlab=xlab,ylab=ylab,log="x")
  lines(b$Q~b$T)
  lines(b$Int1~b$T,lty=2)
  lines(b$Int2~b$T,lty=2)
  legend(aa, c(paste("Komolgorov-Smirnov =",KomolgorovTest),paste("R2 =",r2)),cex=0.8,bty = "n")}

AjustNorm<-function (x,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
mn<-length(x)
Num<-seq(1,mn,1)
a1<-data.frame(Variable=x,Num=Num)
a1$Famp<-(a1$Num-0.3)/(mn+0.4)
a1$Tretor<-1/a1$Famp
a1$Uexp<-qnorm(a1$Famp)
mA<-mean(a1$Variable)
deA<-sd(a1$Variable)
a1$Qteor<-qnorm(a1$Famp,mA,deA)
a1$Fteor<-pnorm(a1$Variable,mA,deA)
a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
KomolgorovTest<-round(max(a1$Komolgorov),3)
a1$Sxp<-sqrt(((deA^2)/mn)*(1+(a1$Uexp^2/2)))
a1$Int1<-a1$Qteor-(2*a1$Sxp)
a1$Int2<-a1$Qteor+(2*a1$Sxp)
F<-c(0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.995,1-(1/max(a1$Tretor)*0.9))
if (type=="min"){T<-1/F} else {T<-1/(1-F)}
b<-data.frame(cbind(F,T))
b$Uf<-qnorm(b$F)
b$Q<-mA+b$Uf*deA
b$Sxp<-sqrt(((deA^2)/mn)*(1+(b$Uf^2/2)))
b$Int1<-b$Q-(2*b$Sxp)
b$Int2<-b$Q+(2*b$Sxp)

r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
list("KomolgorovTest"=KomolgorovTest,"R2"=r2,"values"=a1,"theor"=b)}

AjustSqNormGraph<-function (x,xlab,ylab,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
  if (type=="min"){aa="topleft"}else{aa="bottomleft"}
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  a1$Famp<-(a1$Num-0.3)/(mn+0.4)
  a1$Tretor<-1/a1$Famp
  a1$sqr<-sqrt(a1$Variable)
  a1$Uexp<-qnorm(a1$Famp)
  mA<-mean(a1$sqr)
  deA<-sd(a1$sqr)
  a1$Qsqteor<-qnorm(a1$Famp,mA,deA)
  a1$Qteor<-a1$Qsqteor^2
  a1$Fteor<-pnorm(a1$sqr,mA,deA)
  a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
  KomolgorovTest<-round(max(a1$Komolgorov),3)
  a1$Sxp<-sqrt((4*a1$Qteor*deA^2)/mn)*(1+(a1$Uexp^2/2))
  a1$Int1<-a1$Qteor-(2*a1$Sxp)
  a1$Int2<-a1$Qteor+(2*a1$Sxp)
  F<-c(0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.995,1-(1/max(a1$Tretor)*0.9))
  if (type=="min"){T<-1/F} else {T<-1/(1-F)}
  b<-data.frame(cbind(F,T))
  b$Uf<-qnorm(b$F)
  b$Q<-(mA+b$Uf*deA)^2
  b$Sxp<-sqrt(((deA^2)/mn)*(1+(b$Uf^2/2)))
  b$Sxp<-sqrt((4*b$Q*deA^2)/mn)*(1+(b$Uf^2/2))
  b$Int1<-b$Q-(2*b$Sxp)
  b$Int2<-b$Q+(2*b$Sxp)
  
  r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
  plot(a1$Variable~a1$Tretor,ylim=c(0,max(a1$Variable)*1.05),pch=3,xlim=c(1,max(a1$Tretor)+50),xlab=xlab,ylab=ylab,log="x")
  lines(b$Q~b$T)
  lines(b$Int1~b$T,lty=2)
  lines(b$Int2~b$T,lty=2)
  legend(aa, c(paste("Komolgorov-Smirnov =",KomolgorovTest),paste("R2 =",r2)),cex=0.8,bty = "n")}

AjustSqNorm<-function (x,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  a1$Famp<-(a1$Num-0.3)/(mn+0.4)
  a1$Tretor<-1/a1$Famp
  a1$sqr<-sqrt(a1$Variable)
  a1$Uexp<-qnorm(a1$Famp)
  mA<-mean(a1$sqr)
  deA<-sd(a1$sqr)
  a1$Qsqteor<-qnorm(a1$Famp,mA,deA)
  a1$Qteor<-a1$Qsqteor^2
  a1$Fteor<-pnorm(a1$sqr,mA,deA)
  a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
  KomolgorovTest<-round(max(a1$Komolgorov),3)
  a1$Sxp<-sqrt((4*a1$Qteor*deA^2)/mn)*(1+(a1$Uexp^2/2))
  a1$Int1<-a1$Qteor-(2*a1$Sxp)
  a1$Int2<-a1$Qteor+(2*a1$Sxp)
  F<-c(0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.995,1-(1/max(a1$Tretor)*0.9))
  if (type=="min"){T<-1/F} else {T<-1/(1-F)}
  b<-data.frame(cbind(F,T))
  b$Uf<-qnorm(b$F)
  b$Q<-(mA+b$Uf*deA)^2
  b$Sxp<-sqrt(((deA^2)/mn)*(1+(b$Uf^2/2)))
  b$Sxp<-sqrt((4*b$Q*deA^2)/mn)*(1+(b$Uf^2/2))
  b$Int1<-b$Q-(2*b$Sxp)
  b$Int2<-b$Q+(2*b$Sxp)
  r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
  list("KomolgorovTest"=KomolgorovTest,"R2"=r2,"values"=a1,"theor"=b)}

AjustLogNormGraph<-function (x,xlab,ylab,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
  if (type=="min"){aa="topleft"}else{aa="bottomleft"}
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  a1$Famp<-(a1$Num-0.3)/(mn+0.4)
  a1$Tretor<-1/a1$Famp
  a1$ln<-log(a1$Variable)
  a1$Uexp<-qnorm(a1$Famp)
  mA<-mean(a1$ln)
  deA<-sd(a1$ln)
  a1$Qlnteor<-qnorm(a1$Famp,mA,deA)
  a1$Qteor<-exp(a1$Qlnteor)
  a1$Fteor<-pnorm(a1$ln,mA,deA)
  a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
  KomolgorovTest<-round(max(a1$Komolgorov),3)
  a1$Sxp<-sqrt((a1$Qteor^2*deA^2/mn)*(1+(a1$Uexp^2/2)))
  a1$Int1<-a1$Qteor-(2*a1$Sxp)
  a1$Int2<-a1$Qteor+(2*a1$Sxp)
  F<-c(0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.995,1-(1/max(a1$Tretor)*0.9))
  if (type=="min"){T<-1/F} else {T<-1/(1-F)}
  b<-data.frame(cbind(F,T))
  b$Uf<-qnorm(b$F)
  b$Q<-exp(mA+b$Uf*deA)
  b$Sxp<-sqrt((b$Q^2*deA^2)/mn)*(1+(b$Uf^2/2))
  b$Int1<-b$Q-(2*b$Sxp)
  b$Int2<-b$Q+(2*b$Sxp)
  
  r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
  plot(a1$Variable~a1$Tretor,ylim=c(0,max(a1$Variable)*1.05),pch=3,xlim=c(1,max(a1$Tretor)+50),xlab=xlab,ylab=ylab,log="x")
  lines(b$Q~b$T)
  lines(b$Int1~b$T,lty=2)
  lines(b$Int2~b$T,lty=2)
  legend(aa, c(paste("Komolgorov-Smirnov =",KomolgorovTest),paste("R2 =",r2)),cex=0.8,bty = "n")}

AjustLogNorm<-function (x,type){
  if (type=="min"){x <- x[order(x)] } else {x <- x[order(-x)] }
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  x <- x[order(x)] 
  mn<-length(x)
  Num<-seq(1,mn,1)
  a1<-data.frame(Variable=x,Num=Num)
  a1$Famp<-(a1$Num-0.3)/(mn+0.4)
  a1$Tretor<-1/a1$Famp
  a1$ln<-log(a1$Variable)
  a1$Uexp<-qnorm(a1$Famp)
  mA<-mean(a1$ln)
  deA<-sd(a1$ln)
  a1$Qlnteor<-qnorm(a1$Famp,mA,deA)
  a1$Qteor<-exp(a1$Qlnteor)
  a1$Fteor<-pnorm(a1$ln,mA,deA)
  a1$Komolgorov<-abs(a1$Famp-a1$Fteor)
  KomolgorovTest<-round(max(a1$Komolgorov),3)
  a1$Sxp<-sqrt((a1$Qteor^2*deA^2/mn)*(1+(a1$Uexp^2/2)))
  a1$Int1<-a1$Qteor-(2*a1$Sxp)
  a1$Int2<-a1$Qteor+(2*a1$Sxp)
  ###arreglar por si tretro es inferior a 200
  Fr<-seq(0.0001,0.9999,0.0001)
  if (type=="min"){Ti<-1/Fr} else {Ti<-1/(1-Fr)}
  b<-data.frame(cbind(Fr,Ti))
  b$Uf<-qnorm(b$Fr)
  b$Q<-exp(mA+b$Uf*deA)
  b$Sxp<-sqrt((b$Q^2*deA^2)/mn)*(1+(b$Uf^2/2))
  b$Int1<-b$Q-(2*b$Sxp)
  b$Int2<-b$Q+(2*b$Sxp)
  r2<-round(cor(a1$Qteor,a1$Variable)^2,3)
  list("KomolgorovTest"=KomolgorovTest,"R2"=r2,"values"=a1,"theor"=b)}
