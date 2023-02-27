#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# PLSC 503 - Spring 2023
#
# Model specification / omitted variable bias, 
# plus multiplicative interactions.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, my dude:

P<-c("readr","car","lattice")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# setwd() here:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
# Options:

options(scipen = 9) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Model specification, etc.                         ####
#
# Simulate data:

set.seed(7222009)
N <- 100
X1<-rnorm(N)             # <- X1
X2<-(-X1)+1.5*(rnorm(N)) # <- correlated w/X1
Y<-X1-(2*X2)+(2*(rnorm(N))) # <- Y
Z<- (-2*X1) + rnorm(N)  # <- correlated w/X1 but irrelevant
data <- data.frame(Y=Y,X1=X1,X2=X2,Z=Z)

# Scatterplot matrix:

pdf("MisspecificationExampleScatterplotMatrixR.pdf",7,7)
scatterplotMatrix(data)
dev.off()

# "Correct" model:

correct<-lm(Y~X1+X2)
summary(correct)

# "Overspecified" model:

overspec<-lm(Y~X1+X2+Z)
summary(overspec)

# "Underspecified" model:

incorrect<-lm(Y~X1)
summary(incorrect)

# Omitted variable plot:

pdf("MisspecifiedResidualsR.pdf",6,6)
plot(data$X2,incorrect$residuals,pch=20,
     xlab="Omitted Variable (X2)",ylab="Residuals")
abline(lm(incorrect$residuals~data$X2),lwd=2)
abline(h=0,lty=2)
abline(v=0,lty=2)
dev.off()

# Now repeat that a bunch, to make the point...

reps<-1000
GoodBs<-numeric(reps)
AlsoGoodBs<-numeric(reps)
BadBs<-numeric(reps)
set.seed(7222009)

for(i in 1:reps){
  N <- 100
  X1<-rnorm(N)                # <- X1
  X2<-(-X1)+1.5*(rnorm(N))    # <- correlated w/X1
  Y<-X1-(2*X2)+(2*(rnorm(N))) # <- Y
  Z<- (-2*X1) + rnorm(N)      # <- correlated w/X1 but irrelevant
  data <- data.frame(Y=Y,X1=X1,X2=X2,Z=Z)
  correct<-lm(Y~X1+X2)
  GoodBs[i]<-correct$coefficients[2]
  overspec<-lm(Y~X1+X2+Z)
  AlsoGoodBs[i]<-overspec$coefficients[2]
  incorrect<-lm(Y~X1)
  BadBs[i]<-incorrect$coefficients[2]
  }

pdf("MisspecifiedSimPlot.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(GoodBs),t="l",lwd=2,xlim=c(-1,5),
     xlab="Estimated Beta for X1",ylab="Density",
     main="")
lines(density(AlsoGoodBs),lwd=2,lty=2,col="darkgreen")
lines(density(BadBs),lwd=2,lty=3,col="orange")
abline(v=mean(GoodBs),lty=2,lwd=0.5)
abline(v=mean(AlsoGoodBs),lty=2,lwd=0.5,col="darkgreen")
abline(v=mean(BadBs),lty=2,lwd=0.5,col="orange")
legend("topright",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","darkgreen","orange","white"),
       legend=c("Correctly Specified","Overspecified",
                "Underspecified","(True Beta = 1.0)"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Multiplicative interactions...                    ####
#
# Simulations for pictures....
#
# Two dummy predictors:

set.seed(7222009)
N<-400
D1<-rep(c(0,1),times=N/2)
D2<-rep(c(0,0,1,1),times=N/4)
Y <- rnorm(N,(20-10*D2+10*D1+20*D1*D2),2)
df<-data.frame(D1=D1,D2=D2,Y=Y)

pdf("TwoDummyBoxPlotsRD1.pdf",6,6)
par(mar=c(4,4,2,2))
with(df, boxplot(Y~D2+D1,xaxt="n",xlab="Values of D1,D2"))
axis(1,at=c(1,2,3,4),
     labels=c("D1=0, D2=0","D1=0, D2=1",
              "D1=1, D2=0","D1=1, D2=1"))
arrows(1,median(df$Y[which(df$D1==0 & df$D2==0)]),
       3,median(df$Y[which(df$D1==1 & df$D2==0)]),
       lwd=2,length=0.10,col="red")
arrows(2,median(df$Y[which(df$D1==0 & df$D2==1)]),
       4,median(df$Y[which(df$D1==1 & df$D2==1)]),
       lwd=2,length=0.10,col="red")
legend("topleft",bty="n",legend="E(Y) | change in D1",col="red")
dev.off()

pdf("TwoDummyBoxPlotsRD2.pdf",6,6)
par(mar=c(4,4,2,2))
with(df, boxplot(Y~D2+D1,xaxt="n",xlab="Values of D1,D2"))
axis(1,at=c(1,2,3,4),
     labels=c("D1=0, D2=0","D1=0, D2=1",
                "D1=1, D2=0","D1=1, D2=1"))
arrows(1,median(df$Y[which(df$D1==0 & df$D2==0)]),
       2,median(df$Y[which(df$D1==0 & df$D2==1)]),
       lwd=2,length=0.10)
arrows(3,median(df$Y[which(df$D1==1 & df$D2==0)]),
       4,median(df$Y[which(df$D1==1 & df$D2==1)]),
       lwd=2,length=0.10)
legend("topleft",bty="n",legend="E(Y) | change in D2")
dev.off()

# Dummy + continuous:

set.seed(7222009)
N<-200
D<-rep(c(0,1),times=N/2)
X<-rnorm(N,0,5)
color<-ifelse(D==0,"black","red")
df2<-data.frame(D=D,X=X,color=color,
                stringsAsFactors=FALSE)

df2$Y1 <- 50+2*df2$X+3*rnorm(N)
df2$Y2 <- 50+2*df2$X+30*df2$D+3*rnorm(N)
df2$Y3 <- 50+2*df2$X-(4*df2$D*df2$X)+3*rnorm(N)
df2$Y4 <- 50+2*df2$X+30*df2$D-(4*df2$D*df2$X)+3*rnorm(N)

pdf("ScatterInterSameR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y1,pch=D+16,col=color,ylab="Y"))
legend("topleft",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y1~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y1~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterInterceptR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y2,pch=D+16,col=color,ylab="Y"))
legend("topleft",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y2~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y2~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterSlopeR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y3,pch=D+16,col=color,ylab="Y"))
legend("topright",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y3~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y3~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterBothR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y4,pch=D+16,col=color,ylab="Y"))
legend("topright",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y4~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y4~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

# Two continuous: Wireframe plots...

df3<-expand.grid(X1=seq(0,10,1),
                 X2=seq(0,10,1))
df3$YNoInt<-10 + 2*df3$X1 + 2*df3$X2
df3$YInt  <-(10 - 2*df3$X1 - 2*df3$X2 + 4*df3$X1*df3$X2)/5

trellis.par.set("axis.line",list(col="transparent"))

pdf("TwoContinuousNoInteractiveR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df3, wireframe(YNoInt~X1+X2,
          drape=TRUE,
          xlab=list("X1",rot=30),
          ylab=list("X2",rot=-40),
          zlab=list("Y",rot=90),
          scales=list(arrows=FALSE,col="black"),
          zoom=0.85,pretty=TRUE,
          col.regions=colorRampPalette(c("blue","red"))(100)))
dev.off()

pdf("TwoContinuousInteractiveR.pdf",6,6)
par(mar=c(4,4,2,2))
with(df3, wireframe(YInt~X1+X2,
                    drape=TRUE,
                    xlab=list("X1",rot=30),
                    ylab=list("X2",rot=-40),
                    zlab=list("Y",rot=90),
                    scales=list(arrows=FALSE,col="black"),
                    zoom=0.85,pretty=TRUE,
                    col.regions=colorRampPalette(c("blue","red"))(100)))
dev.off()

# Polynomials...

N<-200
set.seed(7222009)
df4 <- data.frame(X = runif(N,-5,5))
df4$Y2A <- 10 + 1*df4$X - 5*(df4$X^2) + rnorm(N,0,10) # Quad #1
df4$Y2B <- -50 - 1*df4$X + 3*(df4$X^2) + rnorm(N,0,10) # Quad #2
df4$Y3 <- -8 - 6*df4$X + 3*(df4$X^2) + 1*(df4$X^3) + rnorm(N,0,10) # Cubic
df4 <- df4[order(df4$X),]
fitA<-with(df4, lm(Y2A~X+I(X^2)))
fitB<-with(df4, lm(Y2B~X+I(X^2)))
fit3<-with(df4, lm(Y3~X+I(X^2)+I(X^3)))

pdf("TwoQuadraticsR.pdf",7,6)
par(mar=c(4,4,2,2))
with(df4, plot(X,Y2B,pch=16,col="black",ylab="Y",
               ylim=c(min(df4$Y2B),max(df4$Y2A))))
points(df4$X,df4$Y2A,pch=17,col="red")
lines(df4$X,fitted(fitA),lwd=3,col="red")
lines(df4$X,fitted(fitB),lwd=3,col="black")
dev.off()

pdf("CubicR.pdf",7,6)
par(mar=c(4,4,2,2))
with(df4, plot(X,Y3,pch=16,col="black",ylab="Y"))
lines(df4$X,fitted(fit3),lwd=3,col="black")
dev.off()

# Three-way interaction sim:

N <- 100
X <- runif(N,-5,5)
df00<-data.frame(X=X)
df01<-data.frame(X=X)
df10<-data.frame(X=X)
df11<-data.frame(X=X)
set.seed(7222009)
df00$Y<-0.5*df00$X+rnorm(N)
df01$Y<-2*df01$X+rnorm(N)
df10$Y<- -0.5*df10$X+rnorm(N)
df11$Y<- -2*df11$X+rnorm(N)
fit00<-lm(Y~X,data=df00)
fit01<-lm(Y~X,data=df01)
fit10<-lm(Y~X,data=df10)
fit11<-lm(Y~X,data=df11)
hi<-12
lo<- -12

pdf("TwoDummyOneContinuousR.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
with(df00, plot(X,Y,main="D1=0,D2=0",ylab="Y",
          ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit00,lwd=2)
with(df01, plot(X,Y,main="D1=0,D2=1",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit01,lwd=2)
with(df10, plot(X,Y,main="D1=1,D2=0",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit10,lwd=2)
with(df11, plot(X,Y,main="D1=1,D2=1",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit11,lwd=2)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Actual data: Clinton, 1996                          ####

ClintonTherm<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/ClintonTherm.csv")

summary(ClintonTherm)

summary(with(ClintonTherm, lm(ClintonTherm~RConserv+GOP)))

fit1<-with(ClintonTherm, lm(ClintonTherm~RConserv+GOP+RConserv*GOP))
summary(fit1)

# Plot of thermometer scores vs. conservatism:

pdf("ClinThermScatterR.pdf",6,6)
scatterplot(ClintonTherm$ClintonTherm~ClintonTherm$RConserv|as.factor(ClintonTherm$GOP),
            legend.plot=FALSE,
            xlab="Respondent Conservatism",
            ylab="Clinton Thermometer Score",
            smooth=FALSE,boxplots=FALSE,
            pch=c(4,16),col=c("red","blue","red","blue"),
            lwd=2,grid=FALSE)
dev.off()

# Separate regressions:

NonReps<-subset(ClintonTherm,GOP==0)
summary(with(NonReps, lm(ClintonTherm~RConserv)))

Reps<-subset(ClintonTherm,GOP==1)
summary(with(Reps, lm(ClintonTherm~RConserv)))

# psi_1:
Psi1<-fit1$coeff[2]+fit1$coeff[4]
Psi1
SPsi1<-sqrt(vcov(fit1)[2,2] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[2,4])
SPsi1
Psi1 / SPsi1 # <-- t-statistic

# psi_2 | RConserv = 1

fit1$coeff[3]+(1 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[3,4])

# Implies t is approximately 2

# psi_2 | RConserv = 7

fit1$coeff[3]+(7 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (7)^2*vcov(fit1)[4,4] + 2*7*vcov(fit1)[3,4])

# t is approximately 11

# Using linearHypothesis:

linearHypothesis(fit1,"RConserv+RConserv:GOP")

# Note: Same as t-test:
sqrt(72.99)

# psi_2 | RConserv = 7:

linearHypothesis(fit1,"GOP+7*RConserv:GOP")

# MFX / psi plots:

ConsSim<-seq(1,7,1)
psis<-fit1$coeff[3]+(ConsSim * fit1$coeff[4])
psis.ses<-sqrt(vcov(fit1)[3,3] + 
         (ConsSim)^2*vcov(fit1)[4,4] + 2*ConsSim*vcov(fit1)[3,4])

pdf("ClinMFX1.pdf",7,6)
par(mar=c(4,4,2,2))
plot(ConsSim,psis,t="l",lwd=2,xlab="Respondent Conservatism",
       ylab="Estimated Marginal Effect",ylim=c(-40,0))
lines(ConsSim,psis+(1.96*psis.ses),lty=2,lwd=2)
lines(ConsSim,psis-(1.96*psis.ses),lty=2,lwd=2)
abline(h=0,lwd=1,lty=2)
dev.off()

# Continuous covariates:

fit2<-with(ClintonTherm,
      lm(ClintonTherm~RConserv+ClintonConserv+RConserv*ClintonConserv))
summary(fit2)

# Hypothesis tests:

fit2$coef[2]+(1*fit2$coef[4])
sqrt(vcov(fit2)[2,2] + (1)^2*vcov(fit2)[4,4] + 2*1*vcov(fit2)[2,4])

linearHypothesis(fit2,"RConserv+1*RConserv:ClintonConserv")

# More hypothesis tests:

# psi_1 | ClintonConserv = mean
fit2$coef[2]+((mean(ClintonTherm$ClintonConserv))*fit2$coef[4])
sqrt(vcov(fit2)[2,2] + (mean(ClintonTherm$ClintonConserv)^2*vcov(fit2)[4,4] +
                          2*(mean(ClintonTherm$ClintonConserv))*vcov(fit2)[2,4]))
pt(((fit2$coef[2]+(2.985*fit2$coef[4])) / sqrt(vcov(fit2)[2,2] + 
    (2.985)^2*vcov(fit2)[4,4] + 2*2.985*vcov(fit2)[2,4])),df=1293)

# psi_2 | RConserv = 1
fit2$coef[3]+(1*fit2$coef[4])

# psi_2 | RConserv = 6
fit2$coef[3]+(6*fit2$coef[4])

# Marginal Effect Plot II:

psis2<-fit2$coef[3]+(ConsSim*fit2$coef[4])
psis2.ses<-sqrt(vcov(fit2)[3,3] + (ConsSim)^2*vcov(fit2)[4,4]
                  + 2*ConsSim*vcov(fit2)[3,4])

pdf("ClinMFX2.pdf",6,6)
plot(ConsSim,psis2,t="l",lwd=2,xlab="Respondent's Conservatism",
  ylab="Marginal Effect of Clinton's 
  Conservatism",ylim=c(-10,20))
lines(ConsSim,psis2+(1.96*psis2.ses),lty=2,lwd=2)
lines(ConsSim,psis2-(1.96*psis2.ses),lty=2,lwd=2)
abline(h=0,lty=2,lwd=1,col="red")
dev.off()

# Contour Plot:

grid<-expand.grid(RConserv=seq(1,7,1),
                  ClintonConserv=seq(1,7,1))
hats<-predict(fit2,newdata=grid)

pdf("ClinContour.pdf",6,6)
levelplot(hats~grid$RConserv*grid$ClintonConserv,
            contour=TRUE,
            cuts=12,pretty=TRUE,xlab="Respondent's Conservatism",
            ylab="Clinton's Conservatism",
            col.regions=heat.colors)
dev.off()

# Wireframe plot:

trellis.par.set("axis.line",list(col="transparent"))

pdf("ClinWireframe.pdf",7,7)
wireframe(hats~grid$RConserv*grid$ClintonConserv,
            drape=TRUE,
            xlab=list("Respondent's Conservatism",rot=30),
            ylab=list("Clinton's Conservatism",
                      rot=-40),zlab=list("Predictions",rot=90),
            scales=list(arrows=FALSE,col="black"),
            zoom=0.85,pretty=TRUE,
            col.regions=colorRampPalette(c("blue","red"))(100))
dev.off()
