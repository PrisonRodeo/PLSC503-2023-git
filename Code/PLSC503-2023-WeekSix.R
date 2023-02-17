#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# PLSC 503 -- Spring 2023: Code for Week Six
# (outliers / influence + simultaneity/endogeneity)
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages:

P<-c("RCurl","readr","car","psych","MASS","sem",
     "stargazer","gvlma","AER")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen=9) # penalty for scientific notation
options(digits=3)  # significant digits

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# IV Estimation...                                ####
#
# Simulated example:

seed<-1337
set.seed(seed)

mu<-c(0,0,0) # <== X, Z, U
Sigma<-matrix(c(1,0.8,0.4,0.8,1,0,0.4,0,1),
              nrow=3,byrow=TRUE) # Cor(X,Y)=0.8, etc.
Vars<- mvrnorm(500,mu,Sigma)
colnames(Vars)<-c("X","Z","U")
Vars<-data.frame(Vars)

Vars$Y<- 1 + Vars$X + Vars$U

pdf("IVSimScatter.pdf",7,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(Vars,pch=20)
dev.off()

# OLS:

OLS<- lm(Y~X,data=Vars)
summary(OLS)

# 2SLS:

TSLS<-tsls(Y~I(X),data=Vars,instruments=~Z)
summary(TSLS)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Real data" example...                          ####

IRData<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/IRData.csv")

summary(IRData)

# OLS:

OLSWar<-lm(logdisputes~logtrade+contiguity+capratio,
           data=IRData)
summary(OLSWar)

# 2SLS:

TwoSLSWar<-ivreg(logdisputes~contiguity+capratio+I(logtrade),
                 instruments=~contiguity+capratio+IOs,
                 data=IRData)
summary(TwoSLSWar)

# "By hand":

ITrade<-lm(logtrade~contiguity+IOs+capratio,
           data=IRData)
summary(ITrade)
IVWarByHand<-with(IRData,
                  lm(logdisputes~capratio+contiguity+
                       (ITrade$fitted.values)))
summary(IVWarByHand)

# Now, trade:

OLSTrade<-lm(logtrade~logdisputes+contiguity+IOs,
             data=IRData)
summary(OLSTrade)

TwoSLSTrade<-ivreg(logtrade~contiguity+IOs+I(logdisputes),
                   instruments=~contiguity+capratio+IOs,
                   data=IRData)
summary(TwoSLSTrade)

# Summary table:

stargazer(OLSWar,TwoSLSWar,OLSTrade,TwoSLSTrade)


# Plots of IVs:

IWar<-lm(logdisputes~contiguity+IOs+capratio,
         data=IRData)

# The good:

pdf("PrettyGoodInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRData, 
     plot(logtrade,ITrade$fitted.values,pch=20,col="black",
          xlab="Actual Values of ln(Trade)",
          ylab="Predicted Values of ln(Trade)"))
abline(a=0,b=1,lwd=2)
dev.off()

# ...and the bad:

pdf("CrappyInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRdata, 
     plot(logdisputes,IWar$fitted.values,pch=20,col="black",
          ylim=c(-1,3),xlab="Actual Values of ln(Disputes)",
          ylab="Predicted Values of ln(Disputes)"))
abline(a=0,b=1,lwd=2)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Outliers, etc: "Flintstones" data                 ####

flintstones<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/flintstones.csv")

# No Barney OR Dino:
summary(lm(Y~X,data=subset(flintstones,name!="Dino" & name!="Barney")))

# No Barney (Dino included):
summary(lm(Y~X,data=subset(flintstones,name!="Barney")))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dahl data...

NewDahl<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/NewDahl.csv")

NewDahl<-NewDahl[complete.cases(NewDahl),]
row.names(NewDahl)<-NewDahl$Year
summary(NewDahl)

pdf("NewDahlScatterplotMatrix.pdf",10,8)
with(NewDahl, scatterplotMatrix(~NNulls+Age+Tenure+Unified,
                                pch=19,col="black",
                                regLine=list(col="red"),
                                smooth=list(spread=FALSE)))
dev.off()

# Regression:

Fit<-with(NewDahl, lm(NNulls~Age+Tenure+Unified))
summary(Fit)

# Plot over time:

pdf("NewDahlOverTime.pdf",7,6)
par(mar=c(4,4,2,4))
with(NewDahl,plot(Year,NNulls,t="l",lwd=2,ylab="Nullifications"))
par(new=TRUE)
with(NewDahl,plot(Year,Age,t="l",lwd=2,lty=5,col="red",yaxt="n",
                  ylab="",ylim=c(45,72)))
axis(4,xlim=c(45,72),col="red",col.ticks="red",col.axis="red")
mtext("Mean SCOTUS Age",side=4,line=2.5,col="red")
dev.off()

# Outliers...

FitResid<-with(NewDahl, (Fit$model$NNulls - predict(Fit))) # residuals
FitStandard<-rstandard(Fit) # standardized residuals
FitStudent<-rstudent(Fit) # studentized residuals
FitCooksD<-cooks.distance(Fit) # Cook’s D
FitDFBeta<-dfbeta(Fit) # DFBeta
FitDFBetaS<-dfbetas(Fit) # DFBetaS
FitCOVRATIO<-covratio(Fit) # COVRATIOs

# Can also use influence.measures(Fit)

# Plot Studentized residuals:

pdf("StudentResids.pdf",7,6)
plot(NewDahl$Year,FitStudent,pch=19,
     ylab="Studentized Residual",xlab="Year")
text(NewDahl[FitStudent>3,]$Year,FitStudent[FitStudent>3],
     labels=NewDahl[FitStudent>3,]$Year,pos=1,cex=0.9)
abline(h=0,lty=2)
dev.off()

max(FitStudent)
NewDahl$Year1935<-ifelse(NewDahl$Year==1935,1,0)
summary(with(NewDahl, lm(NNulls~Age+Tenure+Unified+Year1935)))

# Influence plot:

pdf("NewInfluencePlot.pdf",7,6)
par(mar=c(4,4,2,2))
influencePlot(Fit,id=list(method="noteworthy",n=4,cex=0.7,
                          labels=NewDahl$Year,col="red"),
              xlab="Leverage")
dev.off()

# DFBETAs plots:

pdf("NewDFBETASPlots.pdf",9,6)
par(mar=c(4,4,2,2))
dfbetasPlots(Fit,id.n=5,id.col="red",main="",pch=19,
             layout=c(1,3),labels=NewDahl$Year)
dev.off()

# COVRATIO Plot:

pdf("NewCOVRATIOPlot.pdf",7,6)
par(mar=c(4,4,2,2))
plot(FitCOVRATIO~NewDahl$Year,pch=19,ylim=c(0.5,1.1),
     xlab="Year",ylab="Value of COVRATIO")
abline(h=1,lty=2)
abline(v=c(1800,1850,1900,1950,2000),lty=2)
text(NewDahl[FitCOVRATIO<0.9,]$Year,FitCOVRATIO[FitCOVRATIO<0.9],
     labels=NewDahl[FitCOVRATIO<0.9,]$Year,pos=1,cex=0.9)
dev.off()

# Refitting, without "outliers":

out1<-c(1935) # one outlier
LD2<-NewDahl[!(NewDahl$Year %in% out1),]
out2<-c(1935,1968,1997,2000) # four outliers
LD3<-NewDahl[!(NewDahl$Year %in% out2),]
Fit2<-lm(NNulls~Age+Tenure+Unified,data=LD2)
Fit3<-lm(NNulls~Age+Tenure+Unified,data=LD3)
# summary(Fit2)
# summary(Fit3)

stargazer(Fit,Fit2,Fit3)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MOAR DAHL (for -glvma-)                         ####
#
# What not to do:

library(gvlma)
Nope <- gvlma(Fit)
display.gvlmatests(Nope)

# Better:

pdf("DahlLMPlots.pdf",10,8)
par(mfrow=c(2,3))
plot(Fit,which=c(1:6),labels.id=rownames(NewDahl),pch=20)
dev.off()

# Unpacking that:
#
# #1: Residuals vs. fitted, same as:

pdf("DahlResidVsFitted.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Fit,which=1,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #2: QQ plot of residuals:

pdf("DahlResidQQPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=2,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #3: Scale-Location plot:

pdf("DahlScaleLocationPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=3,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #4: Cook's Distance (D):

pdf("DahlCooksDPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=4,labels.id=rownames(NewDahl))
dev.off()

# #5: Residuals vs. Leverage:

pdf("DahlResidVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=5,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #6: Cook's D vs. Leverage:

pdf("DahlCooksDVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=6,labels.id=rownames(NewDahl),pch=20)
dev.off()

# /fin