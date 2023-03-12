#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                   ####
#
# PLSC 503 -- Spring 2022
#
# Code for week eight: bootstrapping and missing data...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages (install as necessary):

P<-c("RCurl","readr","car","MASS","scales","boot",
     "simpleboot","stargazer","jtools")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also maybe -setwd- here, or whatever...

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bootstrapping: Normal residuals...             ####

N<-10
reps<-1001

set.seed(7222009)
X<-rnorm(N)
Y<-2+2*X+rnorm(N)
data<-data.frame(Y,X)

pdf("BootScatter1.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=19)
abline(lm(Y~X,data),lwd=2)
dev.off()

fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975)) # <-- 95% c.i.s
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Bootstrap SEs and CIs using the -boot- package

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit)) 
} 

Boot.fit<-boot(data=data, statistic=Bs, 
               R=reps, formula=Y~X)

BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)

# Same, using the -simpleboot- package

Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))

#=-=-=-=-=-=-=
# Plot:

pdf("BootstrapSims.pdf",7,6)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(0,3),xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("topright",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Second set of sims (non-normal errors):        ####

rm(list=ls())

N<-10
reps<-1001

set.seed(2719)
X<-rnorm(N)
ustar<-rgamma(N,shape=0.2,scale=1)*6 # <- skewed residuals
Y<-2+2*X+(ustar-mean(ustar))
data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

# Plot:

pdf("BootScatter2.pdf",5,5)
plot(X,Y,pch=20)
abline(lm(Y~X,data),lwd=2)
dev.off()

# Bootstrap SEs and CIs "by hand"

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975))
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Bootstrap SEs and CIs using the -boot- package

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit)) 
} 

Boot.fit<-boot(data=data, statistic=Bs, 
               R=reps, formula=Y~X)

BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)

# Same, using the -simpleboot- package:

library(simpleboot)
Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))

#=-=-=-=-=-=-=-=-=
# Plot:

pdf("BootstrapSims2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(-1,4.5),
     xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("topright",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# # Bootstrapping: Justice data...
# 
# temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2022-git/master/Data/Justices.csv")
# Justices<-read.csv(text=temp, header=TRUE)
# rm(temp)
# 
# summary(Justices)
# 
# JOLS<-with(Justices, lm(civrts~score))
# JOLShats<-predict(JOLS,interval="confidence")
# summary(JOLS)
# 
# JBoot <- lm.boot(JOLS, reps)
# summary(JBoot)
# 
# Sort<-order(Justices$score)
# 
# pdf("SCOTUS-bootstrap.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(JBoot,xlab="Segal-Cover Score",ylab="Liberal Voting Percent",
#      pch=20,lwd=c(3,2,2))
# lines(sort(Justices$score), JOLShats[Sort,2],col="red",lty=4,lwd=1)
# lines(sort(Justices$score), JOLShats[Sort,3],col="red",lty=4,lwd=1)
# legend("topleft",legend=c("OLS","Bootstrap"),col=c("red","black"),
#        lty=c(4,2),bty="n")
# dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Missing Data + multiple imputation             ####
#
# Start with a "population" (N=1000) of data,
# where Y depends on X and Z:

set.seed(7222009)
Npop <- 1000
X<-runif(Npop,0,10)   # NOTE: X, Z are correlated a bit...
Z<-(0.3*X)+(0.7*runif(Npop,0,10))
Y<-0+(2*X)+(2*Z)+rnorm(Npop,mean=0,sd=4)
DF<-data.frame(X=X,Z=Z,Y=Y)
fit.pop<-lm(Y~X+Z,DF)
summary(fit.pop)

#-=-=-=-=-=-=-=-=-=-=
# Now randomly make ~50% of the values of 
# Y go missing (that is, make some MCAR 
# data):

pmis<-0.50
DF$Ymcar<-rbinom(Npop,1,pmis)
DF$Ymcar<-ifelse(DF$Ymcar==1,NA,DF$Y)

# Regression w/listwise deletion:

fit.s<-lm(Ymcar~X+Z,DF) # <-- looks fine
summary(fit.s)

# Do that 1000 times and plot the results:

reps<-1000
Bx<-numeric(reps)
Bz<-numeric(reps)

for(i in 1:reps){
  DF$Ymcar<-rbinom(Npop,1,pmis)
  DF$Ymcar<-ifelse(DF$Ymcar==1,NA,DF$Y)
  fit<-lm(Ymcar~X+Z,DF)
  Bx[i]<-coefficients(fit)[2]
  Bz[i]<-coefficients(fit)[3]
}

pdf("MCAR-Betas.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(Bx),t="l",lwd=2,xlab="Estimate of Beta for X",
     main="")
abline(v=coefficients(fit.pop)[2],lty=2)
par(mar=c(4,4,2,2))
plot(density(Bz),t="l",lwd=2,xlab="Estimate of Beta for Z",
     main="")
abline(v=coefficients(fit.pop)[3],lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now: Missingness on Y, such that observations with
# higher values of Z are more likely to have missing
# values on Y. This is MAR data...

set.seed(7222009)
DF$Ymar<-rbinom(Npop,1,(DF$Z/10))
DF$Ymar<-ifelse(DF$Ymar==1,NA,DF$Y)

summary(lm(Ymar~X,DF))

# Do that 1000 times, and plot it:

reps<-1000
Bx<-numeric(reps)

for(i in 1:reps){
  DF$Ymar<-rbinom(Npop,1,(DF$Z/10))
  DF$Ymar<-ifelse(DF$Ymar==1,NA,DF$Y)
  fit<-lm(Ymar~X,DF)
  Bx[i]<-coefficients(fit)[2]
}

pdf("MAR-Bad-Betas.pdf",6,5)
par(mar=c(4,4,2,2))
plot(density(Bx),t="l",lwd=2,xlab="Estimate Betas for X",
     xlim=c(1.8,2.7),main="")
abline(v=coefficients(fit.pop)[2],lty=2)
dev.off()

# Now, add Z to the MAR-data model:

summary(lm(Ymar~X+Z,DF))

# Do that 1000 times, and plot it:

reps<-1000
Bx<-numeric(reps)
Bz<-numeric(reps)

set.seed(7222009)

for(i in 1:reps){
  DF$Ymar<-rbinom(Npop,1,(DF$Z/10))
  DF$Ymar<-ifelse(DF$Ymar==1,NA,DF$Y)
  fit<-lm(Ymar~X+Z,DF)
  Bx[i]<-coefficients(fit)[2]
  Bz[i]<-coefficients(fit)[3]
  }

pdf("MAR-Better-Betas.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(Bx),t="l",lwd=2,xlab="Estimated Betas for X",
     main="")
abline(v=coefficients(fit.pop)[2],lty=2)
par(mar=c(4,4,2,2))
plot(density(Bz),t="l",lwd=2,xlab="Estimated Betas for Z",
     main="")
abline(v=coefficients(fit.pop)[3],lty=2)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, informative missingness / MNAR:
#
# Here, Pr(missing Y) depends (stochastically) on 
# both the values of Z and on values of Y itself...

set.seed(7222009)
DF$Yim<-rbinom(Npop,1,rescale(DF$Z-(4*DF$Y)))
DF$Yim<-ifelse(DF$Yim==1,NA,DF$Y)

summary(lm(Yim~X+Z,DF))

# Do that 1000 times, and plot it:

reps<-1000
Bx<-numeric(reps)
Bz<-numeric(reps)

for(i in 1:reps){
  DF$Yim<-rbinom(Npop,1,rescale(DF$Z-(4*DF$Y)))
  DF$Yim<-ifelse(DF$Yim==1,NA,DF$Y)
  fit<-lm(Yim~X+Z,DF)
  Bx[i]<-coefficients(fit)[2]
  Bz[i]<-coefficients(fit)[3]
}

pdf("MNAR-More-Bad-Betas.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(Bx),t="l",lwd=2,xlab="Estimated Betas for X",
     main="")
abline(v=coefficients(fit.pop)[2],lty=2)
par(mar=c(4,4,2,2))
plot(density(Bz),t="l",lwd=2,xlab="Estimated Betas for Z",
     main="")
abline(v=coefficients(fit.pop)[3],lty=2)
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ANES 2020 example:

ANES<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/main/Data/ANES2020.csv")

# Rescale Age:

ANES$RAge10<-ANES$RAge/10

# Example regression model:

fit.all<-lm(BidenTherm~RConservatism+RHHLaborUnion+
            RFemale+RLatino+RAge10+REducation,data=ANES)
# summary(fit.all)

# Create 67% MCAR missingness:

set.seed(7222009)
ANES$noise<-rnorm(nrow(ANES))
ANES.small<-ANES[ANES$noise>0.5,]

fit.mcar<-lm(BidenTherm~RConservatism+RHHLaborUnion+
              RFemale+RLatino+RAge10+REducation,data=ANES.small)
# summary(fit.mcar)

# Now 50% informative missingness: Keep *only* individuals who
# say that they "strongly approve" of how then-President
# Trump is doing his job:

ANES.T<-ANES[ANES$PresApproval==1,]
fit.im<-lm(BidenTherm~RConservatism+RHHLaborUnion+
               RFemale+RLatino+RAge10+REducation,data=ANES.T)

stargazer(fit.all,fit.mcar,fit.im,type="latex")

pdf("ANES-Missingness-Figure.pdf",8,6)
par(mar=c(4,4,2,2))
plot_coefs(fit.all,fit.mcar,fit.im,
           model.names=c("Complete Data","MCAR","Informed Missing"))
dev.off()

# /fin