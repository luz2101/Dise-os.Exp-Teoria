##Novena práctica dirigida,fecha: 13/04/2021 y 14/04/2021

#a.

caso<-read.delim("clipboard")

N<-15##numero de conglomerados
n<-6##numero de conglomerados para la muestra
f<-n/N

RNGkind(sample.kind="Rounding")
set.seed(132)
indices<-sort(sample(1:N,n))
indices

#b. 

names(caso)

muestra<-caso[caso$Pabellones%in%indices,]
muestra

ybar<-tapply(muestra$Permanencia,muestra$Pabellones,mean)
ybar_bar<-mean(ybar)
ybar_bar

vybar<-(1-f)*(var(ybar))/n

eeybar<-sqrt(vybar)
eeybar

cvybar<-(eeybar/ybar_bar)*100
cvybar

#c.estimador de razon

Mi<-table(muestra$Pabellones)
Mi
m<-sum(Mi)

Mbars<-m/n
Ms<-N*Mbars
ybar2<-sum(Mi*ybar)/m
ybar2

#d.

yvar2<-(1-f)*(sum((Mi^2)*((ybar-ybar2)^2)))/(n*(Mbars^2)*(n-1))
eeybar2<-sqrt(yvar2)
eeybar2

LI<-ybar2-(qnorm(1-0.04/2))*eeybar2
LS<-ybar2+(qnorm(1-0.04/2))*eeybar2

round(cbind(LI,LS),3)

#e. Con el estimador promedio ponderado de promedios.

ybar3<-N*sum(Mi*ybar)/(n*Ms)
ybar3

vybar3<-(1-f)*(sum(((Mi*ybar)^2)/(Mbars^2))-n*(ybar3^2))/(n*(n-1))
eeybar3<-sqrt(vybar3)
eeybar3


#f.

pi<-tapply(muestra$Trabajo,muestra$Pabellones,mean)
pi

p1<-mean(pi)
p1

vp1<-(1-f)*var(pi)/n
eep1<-sqrt(vp1)

LI<-p1-(qnorm(1-0.05/2))*eep1
LS<-p1+(qnorm(1-0.05/2))*eep1
round(cbind(LI,LS),3)

#g.proporcion razon

pi<-tapply(muestra$Trabajo,muestra$Pabellones,mean)
pi

p2<-sum(Mi*pi)/m

vp2<-(1-f)*sum((Mi^2)*((pi-p2)^2))/(n*(Mbars^2)*(n-1))
vp2
eep2<-sqrt(vp2)
eep2

LI<-p2-(qnorm(1-0.05/2))*eep2
LS<-p2+(qnorm(1-0.05/2))*eep2
round(cbind(LI,LS),3)

#i.

e<-2 #considerar ese valor
z<-qnorm(1-0.04/2)
d2<-(e/z)^2

yi<-tapply(muestra$Permanencia,muestra$Pabellones,sum)
yi

s2c<-sum((yi-Mi*ybar2)^2)/(n-1)
s2c

n_est<-ceiling((N*s2c)/(N*(Mbars^2)*d2+s2c))
n_est


