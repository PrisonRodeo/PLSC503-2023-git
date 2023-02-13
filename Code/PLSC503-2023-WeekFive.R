#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                ####
#
# PLSC 503 -- Spring 2023: Code for Week Five
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Install packages as needed...

# Packages:

P<-c("RCurl","readr","plyr","car","psych","rms","plm",
     "lmtest","stargazer","MASS","dplyr","glmnet")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# setwd() too...
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")
#
# Options...

options(scipen = 9) # bias against scientific notation
options(digits = 3) # show fewer decimal places
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Handy "robust" summary function for lm...   ####

url_robust<-"https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 2016 ANES Data                              ####
#
# ANES 2016 pilot study aggregation example...

ANES<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/ANES-pilot-2016.csv")

ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)

# States:

ANES$State<-car::recode(ANES$state,
"1='AL';2='AK';4='AZ';5='AR';6='CA';8='CO';9='CT';
10='DE';11='DC';12='FL';13='GA';15='HI';16='ID';17='IL';
18='IN';19='IA';20='KS';21='KY';22='LA';23='ME';24='MD';
25='MA';26='MI';27='MN';28='MS';29='MO';30='MT';31='NE';
32='NV';33='NH';34='NJ';35='NM';36='NY';37='NC';38='ND';
39='OH';40='OK';41='OR';42='PA';44='RI';45='SC';46='SD';
47='TN';48='TX';49='UT';50='VT';51='VA';53='WA';54='WV';
55='WI';56='WY'")

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(State),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE))
summary(StateFT)

respfit<-with(StateFT, lm(meantherm~log(Nresp)))

pdf("StateThermPlot.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",xlim=c(0.5,200),
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# What do "robust" SEs do? A simulation...    ####

set.seed(7222009)
X <- rnorm(10)
Y <- 1 + X + rnorm(10)
df10 <- data.frame(ID=seq(1:10),X=X,Y=Y)

fit10 <- lm(Y~X,data=df10)
summary(fit10)
rob10 <- vcovHC(fit10,type="HC1")
sqrt(diag(rob10))

# "Clone" each observation 100 times

df1K <- df10[rep(seq_len(nrow(df10)), each=100),]
df1K <- pdata.frame(df1K, index="ID")

fit1K <- lm(Y~X,data=df1K)
summary(fit1K)
summary(fit1K, cluster="ID")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Make the 2016 NES state-level data for a regression:

ANES$RConserv<-ANES$lcself
ANES$RConserv<-ifelse(ANES$RConserv==8,NA,ANES$RConserv)
ANES$BornAgain<-ANES$pew_bornagain
ANES$BornAgain<-ifelse(ANES$BornAgain==8,NA,ANES$BornAgain)
ANES$BornAgain<- 1 - (ANES$BornAgain-1)
ANES$Age<-2016-ANES$birthyr
ANES$Education<-ANES$educ

StateData<-ddply(ANES,.(State),summarise,
               NResp=sum(one),
               LGBTTherm=mean(ftgay,na.rm=TRUE),
               MeanCons=mean(RConserv,na.rm=TRUE),
               MeanAge=mean(Age/10,na.rm=TRUE),
               MeanEducation=mean(Education,na.rm=TRUE),
               BornAgainProp=mean(BornAgain,na.rm=TRUE))
describe(StateData)

# Models:

OLSfit<-with(StateData,lm(LGBTTherm~MeanCons))
summary(OLSfit)

WLSfit<-with(StateData,lm(LGBTTherm~MeanCons,weights=log(NResp)))
summary(WLSfit)

pdf("NES-WLSBubblePlotR.pdf",6,6)
par(mar=c(4,4,2,2))
with(StateData, symbols(MeanCons,LGBTTherm,circles=NResp,
        ylab="Mean LGBT Feeling Thermometer",
        xlab="Mean Respondent Conservatism",
        ylim=c(0,100)))
abline(reg=OLSfit,lwd=2)
abline(reg=WLSfit,lwd=2,lty=2)
with(StateData, text(MeanCons,LGBTTherm,labels=State,cex=0.4))
legend("topleft",bty="n",lty=c(1,2),lwd=2,
       legend=c("OLS","WLS"))
dev.off()

# "Robust" SEs...
#
# Fit a basic multivariate model:

OLS<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+BornAgainProp,
        data=StateData)
summary(OLS)

hccm(OLS,type="hc0") # "HC0" var-cov matrix
sqrt(diag(hccm(OLS,type="hc0"))) # "HC0" robust SEs

# Now more...

WLS<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+BornAgainProp,
        data=StateData,weight=log(NResp))
summary(WLS)

# Comparing different "robust" estimators (a la Long
# & Ervin):

# OLSBs<-coef(OLS)
# Naive<-sqrt(diag(vcov(OLS)))
# HC0<-sqrt(diag(hccm(OLS, type="hc0")))
# HC1<-sqrt(diag(hccm(OLS, type="hc1")))
# HC2<-sqrt(diag(hccm(OLS, type="hc2")))
# HC3<-sqrt(diag(hccm(OLS, type="hc3")))
# WLSBs<-coef(WLS)
# WLS<-sqrt(diag(vcov(WLS)))

# Plot ("by-hand," sorta...):
pd<-data.frame(Beta=c("Intercept","Mean Conservatism","Mean Age / 10",
                            "Mean Education","Proportion Born-Again"),
                     OLSB=coef(OLS),
                     WLSB=coef(WLS),
                     OLSSE=sqrt(diag(vcov(OLS))),
                     WLSSE=sqrt(diag(vcov(WLS))),
                     OLSHC0=sqrt(diag(hccm(OLS,type="hc0"))),
                     OLSHC1=sqrt(diag(hccm(OLS,type="hc1"))),
                     OLSHC2=sqrt(diag(hccm(OLS,type="hc2"))),
                     OLSHC3=sqrt(diag(hccm(OLS,type="hc3"))))
pd<-pd[2:5,]

pdf("RobustSEComparison.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(1:4)-0.2,pd$OLSB,pch=20,xaxt="n",
     xlim=c(0.5,4.5),ylim=c(-26,25),
     xlab="Variables",ylab="Estimates")
axis(1,at=c(1:4)+0.05,labels=pd$Beta,cex.axis=0.7)
segments(rep(1:4)-0.2,pd$OLSB-(1.96*pd$OLSSE),
         rep(1:4)-0.2,pd$OLSB+(1.96*pd$OLSSE))
points(rep(1:4)-0.1,pd$WLSB,pch=17) # WLS
segments(rep(1:4)-0.1,pd$WLSB-(1.96*pd$WLSSE),
         rep(1:4)-0.1,pd$WLSB+(1.96*pd$WLSSE))
points(rep(1:4),pd$OLSB,pch=15,col="red") # HC0
segments(rep(1:4),pd$OLSB-(1.96*pd$OLSHC0),
         rep(1:4),pd$OLSB+(1.96*pd$OLSHC0),col="red")
points(rep(1:4)+0.1,pd$OLSB,pch=18,col="blue") # HC1
segments(rep(1:4)+0.1,pd$OLSB-(1.96*pd$OLSHC1),
         rep(1:4)+0.1,pd$OLSB+(1.96*pd$OLSHC1),col="blue")
points(rep(1:4)+0.2,pd$OLSB,pch=4,col="darkgreen") # HC2
segments(rep(1:4)+0.2,pd$OLSB-(1.96*pd$OLSHC2),
         rep(1:4)+0.2,pd$OLSB+(1.96*pd$OLSHC2),col="darkgreen")
points(rep(1:4)+0.3,pd$OLSB,pch=25,col="orange") # HC2
segments(rep(1:4)+0.3,pd$OLSB-(1.96*pd$OLSHC3),
         rep(1:4)+0.3,pd$OLSB+(1.96*pd$OLSHC3),col="orange")
abline(h=0,lty=2)
legend("topleft",bty="n",pch=c(20,17,15,18,4,25),cex=0.9,
       legend=c("OLS","WLS","HC0","HC1","HC2","HC3"),
       col=c("black","black","red","blue","darkgreen","orange"))
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Collinearity, etc.                          ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Africa data:

Africa<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/africa2001.csv")

# Perfect multicollinearity:

Africa$newgdp<-(Africa$gdppppd-mean(Africa$gdppppd))*1000

fit<-with(Africa, lm(adrate~gdppppd+newgdp+healthexp+subsaharan+
                       muslperc+literacy))
summary(fit)

# N = K

smallAfrica<-subset(Africa,subsaharan=="Not Sub-Saharan")
fit2<-with(smallAfrica,lm(adrate~gdppppd+healthexp+muslperc+
                            literacy+war))
summary(fit2)

# Multicollinearity examples:

with(Africa, table(internalwar,intensity))

HIV1<-with(Africa, lm(adrate~internalwar))
HIV2<-with(Africa, lm(adrate~intensity))
HIV3<-with(Africa, lm(adrate~internalwar+intensity))

stargazer(HIV1,HIV2,HIV3)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Impeachment example...                      ####

impeachment<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/impeachment.csv")

summary(impeachment)

# Standardize all the variables:

ImpStd<-data.frame(scale(impeachment[,4:9]))
cor(ImpStd)

# OLS w/o intercept:

fit<-with(ImpStd,lm(votesum~pctbl96+unionpct+clint96+GOPmember+ADA98-1))
summary(fit)

vif(fit)

# Ridge regression:

X<-ImpStd[,2:6] # Predictors
Y<-ImpStd[,1]   # Response

ridge.fit<-glmnet(X,Y,alpha=0)

pdf("RidgeRegressions.pdf",10,6)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(ridge.fit,label=TRUE)
abline(h=0,lty=2)
matplot(log(ridge.fit$lambda), t(ridge.fit$beta),
        type="l",main="",lwd=2,
        xlab=expression(Log(lambda)),
        ylab=expression("Estimates of"~beta))
abline(h=0,lty=2)
legend("topright",bty="n",col=c("black","red","green3","blue","cyan"),
       lty=c(1,2,3,4,5),legend=c("Percent Black","Union Percent",
                                 "Clinton Percent","GOP","ADA Score"))
dev.off()

# Lasso:

lasso.fit<-glmnet(X,Y,alpha=1)

pdf("LassoRegressions.pdf",10,6)
par(mar=c(4,4,2,2))
par(mfrow=c(1,2))
plot(lasso.fit,label=TRUE)
abline(h=0,lty=2)
matplot(log(lasso.fit$lambda), t(lasso.fit$beta),
        type="l",main="",lwd=2,xlim=c(-6,0.5),
        xlab=expression(Log(lambda)),
        ylab=expression("Estimates of"~beta))
abline(h=0,lty=2)
legend("topright",bty="n",col=c("black","red","green3","blue","cyan"),
       lty=c(1,2,3,4,5),legend=c("Percent Black","Union Percent",
                                 "Clinton Percent","GOP","ADA Score"),
       cex=0.7)
dev.off()

# Cross-validation to get lambda...
#
# Ridge regression:

ridge.cv<-cv.glmnet(as.matrix(X),as.matrix(Y),alpha=0,intercept=FALSE)
ridge.cv
coef(ridge.cv,s="lambda.min")
coef(ridge.cv,s="lambda.1se")

# Lasso:

lasso.cv<-cv.glmnet(as.matrix(X),as.matrix(Y),alpha=1,intercept=FALSE)
lasso.cv
coef(lasso.cv,s="lambda.min")
coef(lasso.cv,s="lambda.1se")

# Plots:

pdf("GLMNET-CVPlots.pdf",10,6)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(ridge.cv)
legend("bottomright",bty="n",legend="Ridge Regression",cex=1.4)
plot(lasso.cv)
legend("topleft",bty="n",legend="Lasso",cex=1.4)
dev.off()

# /fin