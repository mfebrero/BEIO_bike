library(fda.usc) #better install from GitHub.  remotes::install_github("moviedo5/fda.usc")
# Lectura de datos
data=read.csv("./hour.csv")
l=which(data$weekday==6)
datan=data[l,]
tab=table(datan$dteday,datan$hr)
dayf=unique(rownames(which(tab==0,arr.ind=TRUE)))
l=which(data$weekday==6 & !(data$dteday %in% dayf))
datan=data[l,]
ddays=unique(datan$dteday)
#lesp=which(ddays %in% c("2011-09-17","2012-01-21","2012-06-09"))
 tmin=-8; tmax=39
 atmin=-16; atmax=50
 fhum=100
 fws=67
# Conversión a datos funcionales
tj=seq(0.5,23.5)
y0=fdata(matrix(datan$casual,ncol=24,byrow=TRUE),tj,c(0,24),
		names=list(main="NBCR",xlab="Hora",ylab="NBCR"))
nbcr=apply(y0$data,1,sum)
Temp=fdata(matrix(tmin+datan$temp*(tmax-tmin),ncol=24,byrow=TRUE),tj,c(0,24),
		names=list(main="Temperatura",xlab="Hora",ylab="ºC"))
Hum=fdata(matrix(fhum*datan$hum,ncol=24,byrow=TRUE),tj,c(0,24),
		names=list(main="Humedad",xlab="Hora",ylab=""))
WS=fdata(matrix(fws*datan$windspeed,ncol=24,byrow=TRUE),tj,c(0,24),
		names=list(main="Vel. Viento",xlab="Hora",ylab=""))
FT=fdata(matrix(atmin+datan$atemp*(atmax-atmin),ncol=24,byrow=TRUE),tj,c(0,24),
		names=list(main="Sensación térmica",xlab="Hora",ylab="ºC"))
ldatm=ldata(df=data.frame(nbcr=nbcr,lnbcr=log(nbcr)),y=y0,Temp=Temp,Hum=Hum,WS=WS,FT=FT)

source("color.bar.R")
pdf(file="bikerental.pdf",width=10.67,height=6)
ncolor=cut(log(nbcr),5)
colores=colorRampPalette(c("blue","orange"))(length(levels(ncolor)))
#par(mfrow=c(2,2))
par(mfrow=c(1,2))
#plot(Temp,col=colores,ylim=c(0,50),lwd=2)
#color.bar(colores,min(log(nbcr)),max(log(nbcr)))
plot(Hum,col=colores,ylim=c(0,105),lwd=2)
color.bar(colores,min(log(nbcr)),max(log(nbcr)),xpos=0)
#plot(WS,col=colores,ylim=c(0,60),lwd=2)
#color.bar(colores,min(log(nbcr)),max(log(nbcr)))
plot(FT,col=colores,ylim=c(-10,50),lwd=2)
color.bar(colores,min(log(nbcr)),max(log(nbcr)))
dev.off()

# base de componentes principales
b.x=list(Temp=create.pc.basis(ldatm$Temp,1:6),Hum=create.pc.basis(ldatm$Hum,1:3),
		WS=create.pc.basis(ldatm$WS,1:6),FT=create.pc.basis(ldatm$FT,1:6))

#rTemp=fregre.lm(lnbcr~Temp,data=ldatm,basis.x=b.x)
rHum=fregre.lm(lnbcr~Hum,data=ldatm,basis.x=b.x)
#rWS=fregre.lm(lnbcr~WS,data=ldatm,basis.x=b.x)
rFT=fregre.lm(lnbcr~FT,data=ldatm,basis.x=b.x)

summary(rHum)
summary(rFT)

pdf(file="FTregre.pdf",width=10.67,height=6)
par(mfrow=c(1,2))
plot(rHum,which=1,main="Humedad")
plot(rFT,which=1,main="Sensación térmica")
dev.off()

out.Hum=rp.flm.test(Hum,log(nbcr),B=2000)
out.Hum$p.values.fdr
out.FT=rp.flm.test(FT,log(nbcr),B=2000)
out.FT$p.values.fdr
#out.WS=rp.flm.test(WS,log(nbcr),B=2000)

#CvM projections Hum
par(mfrow=c(2,5))
for (i in 1:10) {
	rr=range(c(out.Hum$boot.proj.statistics[i,1,],out.Hum$proj.statistics[i,1]))
	plot(density(out.Hum$boot.proj.statistics[i,1,]),main=paste0("Hum/CvM - R.P.:",i),xlim=rr)
	abline(v=out.Hum$proj.statistics[i,1],col="red")
	}

#KS projections FT
par(mfrow=c(2,5))
for (i in 1:10) {
	rr=range(c(out.Hum$boot.proj.statistics[i,2,],out.Hum$proj.statistics[i,2]))
	plot(density(out.Hum$boot.proj.statistics[i,2,]),main=paste0("Hum/KS - R.P.:",i),xlim=rr)
	abline(v=out.Hum$proj.statistics[i,2],col="red")
	}

#CvM projections FT
par(mfrow=c(2,5))
for (i in 1:10) {
	rr=range(c(out.FT$boot.proj.statistics[i,1,],out.FT$proj.statistics[i,1]))
	plot(density(out.FT$boot.proj.statistics[i,1,]),main=paste0("FT/CvM - R.P.:",i),xlim=rr)
	abline(v=out.FT$proj.statistics[i,1],col="red")
	}

#KS projections FT
par(mfrow=c(2,5))
for (i in 1:10) {
	rr=range(c(out.FT$boot.proj.statistics[i,2,],out.FT$proj.statistics[i,2]))
	plot(density(out.FT$boot.proj.statistics[i,2,]),main=paste0("FT/KS - R.P.:",i),xlim=rr)
	abline(v=out.FT$proj.statistics[i,2],col="red")
	}


# Modelo final

rmod=fregre.gsam(lnbcr~Hum+s(FT),data=ldatm,basis.x=b.x)
summary(rmod)

# Diagnósticos gráficos
par(mfrow=c(1,3))
plot(rmod$y~rmod$fitted.values,xlab="Fitted values",ylab="Response",main="log(NBCR)~Hum+f(ST)",
sub=paste0(	"R^2=",round(1 - var((as.numeric(rmod$y) - rmod$fitted.values)) *
        (length(rmod$y) - 1)/(var((as.numeric(rmod$y) - mean(rmod$y))) *
        rmod$df.residual),3)))

plot(rmod$residuals~rmod$fitted.values,xlab="Fitted values",ylab="Residuals",main="log(NBCR)~Hum+f(ST)")
lines(lowess(rmod$fitted.values,rmod$residuals),lwd=2,col="red")

yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = "Std. resid.")))
resid.sd=abs(rmod$residuals)/sd(rmod$residuals)
plot(sqrt(resid.sd)~rmod$fitted.values,xlab="Fitted values",ylab=yl,main="log(NBCR)~Hum+f(ST)",sub="Scale-Location")
lines(lowess(rmod$fitted.values,sqrt(resid.sd)),lwd=2,col="red")


