#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                ####
#
# PLSC 503 -- Spring 2022: Code for Week Four
#
# Dichotomous predictors and transformations
# (ideally, towards linearity).
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# setwd() here:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
# Packages:

P<-c("readr","car","psych","fastDummies")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen=9) # penalty for scientific notation
options(digits=3)  # significant digits

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ugly dummy variable scatterplot:

set.seed(7222009)
X <- rbinom(100,1,0.5)
Y <- 10 + 10*X + (5*rnorm(100))
Dfit <- lm(Y~X)
Dhat <- predict(Dfit)

pdf("UglyDummyScatterplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
plot(X,Y,pch=20,xaxt="n")
axis(1,at=c(0,1))
points(X,Dhat,pch=4,col="red",cex=2)
abline(Dfit,lwd=2,col="red",lty=2)
legend("bottomright",legend="indicates E(Y|X=0/1)",
       bty="n",col="red",pch=4)
dev.off()

# Better boxplot:

pdf("DummyBoxplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
boxplot(Y~X)
points(X+1,Dhat,pch=4,col="red",cex=2)
abline(a=Dfit$coefficients[1]-Dfit$coefficients[2],
       b=Dfit$coefficients[2],
       lwd=2,col="red",lty=2)
legend("bottomright",legend="indicates E(Y|X=0/1)",
       bty="n",col="red",pch=4)
dev.off()

# Exclusive / exhaustive dummies / factors and 
# intercepts:

labs<-c(rep("A",3),rep("B",3),rep("C",3)) # Three groups
D<-as.factor(labs)                        # "Factor" variable
Y<-c(12,16,8,25,27,23,38,42,40)           # Y 
df<-data.frame(D=D,Y=Y)
df

# Means of Y by group:

aggregate(df$Y,list(df$D),FUN=mean)

# Create binary indicators "by hand":

df$DA<-ifelse(df$D=="A",1,0)
df$DB<-ifelse(df$D=="B",1,0)
df$DC<-ifelse(df$D=="C",1,0)
df

# Same thing, using fastDummies:

df2<-dummy_cols(df[,1:2],select_columns=c("D"))
df2

# Regressions:

summary(lm(Y~D,data=df))
summary(lm(Y~DA+DB+DC,data=df))
summary(lm(Y~DA+DB+DC-1,data=df)) # "-1" removes the constant

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ordinal predictor example:

set.seed(7222009)
Z<-runif(200,0,50)
PID<-ceiling((Z)/10)
Therm<-10*I(X==1)+45*(I(X==2))+50*(I(X==3))+
  55*(I(X==4))+90*(I(X==5))+rnorm(200,0,5)
PID<-factor(PID,ordered=TRUE,
            labels=c("SD","WD","Ind","WR","SR"))

pdf("OrdinalDummyBoxplot.pdf",7,6)
par(mar=c(4,4,2,2))
plot(PID,Therm,xlab="PID",ylab="Thermometer",ylim=c(0,100))
dev.off()

# Regressions:

fit1<-lm(Therm~as.numeric(PID))
summary(fit1)

fit2<-lm(Therm~PID-1)
summary(fit2)

# ... and a plot:

THat<-as.numeric(names(table(predict(fit2))))

pdf("OrdinalDummyBoxplot2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(PID,Therm,xlab="PID",ylab="Thermometer",ylim=c(0,100))
abline(fit1,lwd=2,lty=2,col="navy")
points(c(1:5),THat,pch=19,col="orange")
segments(c(1:4),THat[1:4],c(2:5),THat[2:5],
         lwd=2,col="orange")
legend("topleft",bty="n",col=c("navy","orange"),
       lwd=2,lty=c(2,1),legend=c("Linear Fit","Dummies"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SCOTUS (1953-1985) data...                  ####

SCOTUS<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/SCOTUS.csv")

summary(SCOTUS)

SCOTUS$civil.econ<-SCOTUS$civlibs + SCOTUS$econs

SCOTUS$termdummies<-factor(SCOTUS$term)

fit1<-with(SCOTUS, lm(Namici~civlibs))
summary(fit1)
with(SCOTUS, t.test(Namici~civlibs))

SCOTUS$civlibeffect<-SCOTUS$civlibs
SCOTUS$civlibeffect[SCOTUS$civlibs==0]<-(-1)
fit2<-with(SCOTUS, lm(Namici~SCOTUS$civlibeffect))
summary(fit2)

fit3<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib))
summary(fit3)

fit4<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib+term))
summary(fit4)

fit5<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                     econs+constit+lctlib+as.factor(term)))
summary(fit5)

# Plot coefficients:

termbetas<-fit5$coefficients[8:39]
SE5<-sqrt(diag(vcov(fit5)))[8:39]
termUBs <- termbetas + 1.96*(SE5)
termLBs <- termbetas - 1.96*(SE5)
term<-seq(1954,1985)

pdf("TermBetasR.pdf",6,5)  
par(mar=c(4,4,2,2))
plot(term,termbetas, xlab="Term",ylab="Estimated Betas",
     pch=19,ylim=c(-0.5,2.1))
lines(term,termbetas,lwd=2)
segments(term,termLBs,term,termUBs,lwd=2,lty=2)
abline(h=0,lwd=2,lty=2,col="red")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Transformations...                          ####
#
# What difference? Non-Linearity plots:

set.seed(7222009)
X <- runif(200,-4,4)
Y <- exp(X)+runif(200,0,20)
fit <- lm(Y~X)

pdf("NonLinMisspec1.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(X,Y,pch=20)
abline(lm(Y~X),lwd=3)
segments(X,Y,X,fit$fitted.values,lwd=1,lty=2,col="red")
legend("topleft",bty="n",legend="Regression")
plot(X,fit$residuals,ylab="Residuals",pch=20)
abline(h=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Residuals vs. X")
dev.off()

pdf("NonLinMisspec2.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(fit$residuals),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
qqPlot(fit$residuals,xlab="Normal Quantiles",ylab="Residuals",
       pch=20)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulations for non-normal residuals:

sims <- 1000
set.seed(7222009)
N <- 20  # small sample
NN <- 2000 # big sample...

OKBs <- numeric(sims)
BadBs <- numeric(sims)
OKTs <- numeric(sims)
BadTs <- numeric(sims)

for (i in 1:sims) {
  u <- rnorm(N,0,2) # mean zero, s.d = 2
  # Exponentiate:
  eu <- exp(u)
  eu <- eu-mean(eu) # new residuals are mean-zero
  eu <- (eu/sd(eu))*2 # and also sd = 2
  
  # Generate Ys:
  
  X <- runif(N,-4,4)
  Y1 <- 0 + 1*X + 1*u 
  Y2 <- 0 + 1*X + 1*eu # same Xs in both
  
  fit1 <- lm(Y1~X)
  fit2 <- lm(Y2~X)
  
  OKBs[i] <- fit1$coefficients[2]
  BadBs[i] <- fit2$coefficients[2]
  OKTs[i] <- summary(fit1)$coefficients[2,3]
  BadTs[i] <- summary(fit2)$coefficients[2,3]
} 

# Plots...

pdf("NonLinBadResids.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(u),main=" ",xlab="Residual Values",ylim=c(0,0.25))
abline(v=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals")
plot(density(eu),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals")
dev.off()

pdf("NonLinBsAreJustFine.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(OKBs),main=" ",xlab="Estimated Slope",ylim=c(0,3.5))
abline(v=1,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals",cex=0.85)
plot(density(BadBs),main=" ",xlab="Estimated Slope",ylim=c(0,3))
abline(v=1,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals",cex=0.85)
dev.off()

pdf("NonLinBadTs.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OKTs,BadTs,pch=20,xlab="T-statistics: Normal residuals",
     ylab="T-statistics: Skewed residuals")
abline(a=0,b=1,lwd=2,col="black")
abline(lm(BadTs~OKTs),lwd=2,col="red",lty=2)
legend("topleft",bty="n",lwd=c(2,2),col=c("black","red"),
       lty=c(1,2),legend=c("45-degree line","OLS fit"))
text(12,2,"N=20")
dev.off()

# Now, repeat the entire thing for N = 2000:

set.seed(7222009)
for (i in 1:sims) {
  u <- rnorm(NN,0,2) # mean zero, s.d = 2
  # Exponentiate:
  eu <- exp(u)
  eu <- eu-mean(eu) # new residuals are mean-zero
  eu <- (eu/sd(eu))*2 # and also sd = 2
  
  # Generate Ys:
  
  X <- runif(NN,-4,4)
  Y1 <- 0 + 1*X + 1*u 
  Y2 <- 0 + 1*X + 1*eu # same Xs in both
  
  fit1 <- lm(Y1~X)
  fit2 <- lm(Y2~X)
  
  OKBs[i] <- fit1$coefficients[2]
  BadBs[i] <- fit2$coefficients[2]
  OKTs[i] <- summary(fit1)$coefficients[2,3]
  BadTs[i] <- summary(fit2)$coefficients[2,3]
} 

# Plots...

pdf("NonLinBadResidsNN.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(u),main=" ",xlab="Residual Values",ylim=c(0,0.25))
abline(v=0,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals")
plot(density(eu),main=" ",xlab="Residual Values")
abline(v=0,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals")
dev.off()

pdf("NonLinBsAreJustFineNN.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(OKBs),main=" ",xlab="Estimated Slope")
abline(v=1,lwd=2,lty=2)
legend("topleft",bty="n",legend="Normal Residuals",cex=0.85)
plot(density(BadBs),main=" ",xlab="Estimated Slope")
abline(v=1,lwd=2,lty=2)
legend("topright",bty="n",legend="Skewed Residuals",cex=0.85)
dev.off()

pdf("NonLinBadTsNN.pdf",6,5)
par(mar=c(4,4,2,2))
plot(OKTs,BadTs,pch=20,xlab="T-statistics: Normal residuals",
     ylab="T-statistics: Skewed residuals")
abline(a=0,b=1,lwd=2,col="black")
abline(lm(BadTs~OKTs),lwd=2,col="red",lty=2)
legend("topleft",bty="n",lwd=c(2,2),col=c("black","red"),
       lty=c(1,2),legend=c("45-degree line","OLS fit"))
text(55,48.2,"N=2000")
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Real-Data Example:                          ####

WDI<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/WDI2018.csv")

with(WDI, describe(MobileCellSubscriptions))
with(WDI, describe(GDPPerCapita))

# Ladder Plot (can also use -ladder- in the HH package):

pdf("LadderOfPowersDensitiesWealth.pdf",7,6)
par(mar=c(4,4,2,2))
par(mfrow=c(3,3))
with(WDI, plot(density(GDPPerCapita^3,na.rm=TRUE),main="Cubic",xlab=""))
with(WDI, plot(density(GDPPerCapita^2,na.rm=TRUE),main="Square",xlab=""))
with(WDI, plot(density(GDPPerCapita,na.rm=TRUE),main="Identity",xlab=""))
with(WDI, plot(density(sqrt(GDPPerCapita),na.rm=TRUE),main="Square Root",xlab=""))
with(WDI, plot(density(log(GDPPerCapita),na.rm=TRUE),main="Log",xlab=""))
with(WDI, plot(density(1/sqrt(GDPPerCapita),na.rm=TRUE),main="1 / Square Root",
               xlab=""))
with(WDI, plot(density(1/GDPPerCapita,na.rm=TRUE),main="Inverse",xlab=""))
with(WDI, plot(density(1/GDPPerCapita^2,na.rm=TRUE),main="1 / Square",xlab=""))
with(WDI, plot(density(1/GDPPerCapita^3,na.rm=TRUE),main="1 / Cubic",xlab=""))
dev.off()

# Mobile subscriptions:

pdf("LadderOfPowersDensitiesPhones.pdf",7,6)
par(mar=c(4,4,2,2))
par(mfrow=c(3,3))
with(WDI, plot(density(MobileCellSubscriptions^3,na.rm=TRUE),
               main="Cubic",xlab=""))
with(WDI, plot(density(MobileCellSubscriptions^2,na.rm=TRUE),
               main="Square",xlab=""))
with(WDI, plot(density(MobileCellSubscriptions,na.rm=TRUE),
               main="Identity",xlab=""))
with(WDI, plot(density(sqrt(MobileCellSubscriptions),na.rm=TRUE),
               main="Square Root",xlab=""))
with(WDI, plot(density(log(MobileCellSubscriptions),na.rm=TRUE),
               main="Log",xlab=""))
with(WDI, plot(density(1/sqrt(MobileCellSubscriptions),na.rm=TRUE),
               main="1 / Square Root",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions,na.rm=TRUE),
               main="Inverse",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions^2,na.rm=TRUE),
               main="1 / Square",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions^3,na.rm=TRUE),
               main="1 / Cubic",xlab=""))
dev.off()

# Scatterplots:

dot<-20 # symbol for -pch()-

pdf("WealthPhoneScatters.pdf",7,6)
par(mar=c(4,4,4,2))
par(mfrow=c(2,2))
with(WDI, plot(GDPPerCapita,MobileCellSubscriptions,pch=dot,
               main="Linear-Linear"))
with(WDI, plot(log(GDPPerCapita),MobileCellSubscriptions,pch=dot,
               main="Linear-Log"))
with(WDI, plot(GDPPerCapita,log(MobileCellSubscriptions),pch=dot,
               main="Log-Linear"))
with(WDI, plot(log(GDPPerCapita),log(MobileCellSubscriptions),pch=dot,
               main="Log-Log"))
dev.off()

# Regressions:

linlin <- with(WDI, lm(MobileCellSubscriptions~I(GDPPerCapita/1000)))
summary(linlin)
linlog <- with(WDI, lm(MobileCellSubscriptions~log(GDPPerCapita/1000)))
summary(linlog)
loglin <- with(WDI, lm(log(MobileCellSubscriptions)~I(GDPPerCapita/1000)))
summary(loglin)
loglog <- with(WDI, lm(log(MobileCellSubscriptions)~log(GDPPerCapita/1000)))
summary(loglog)

# Residual plot:

pdf("PhoneResidsDensities.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(linlin$residuals),xlim=c(-80,80),ylim=c(0,0.017),
     lwd=2,lty=1,col="black",main="Untransformed Y",
     xlab="Residual Values")
lines(density(linlog$residuals),lwd=2,lty=2,col="orange")
abline(v=0,lwd=1,lty=2)
legend("topright",bty="n",lwd=2,lty=c(1,2),col=c("black","orange"),
       legend=c("Untransformed X","Transformed X"),cex=0.6)
plot(density(loglin$residuals),xlim=c(-2,2),ylim=c(0,1.7),
     lwd=2,lty=1,col="black",main="Transformed Y",
     xlab="Residual Values")
lines(density(loglog$residuals),lwd=3,lty=4,col="orange")
abline(v=0,lty=2,lwd=1)
legend("topright",bty="n",lwd=2,lty=c(1,2),col=c("black","orange"),
       legend=c("Untransformed X","Transformed X"),cex=0.6)
dev.off()

# fin
