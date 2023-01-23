#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials                               ####
#
# PLSC 503 -- Spring 2023
#
# Code for Week Two ("Multivariate Regression")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# setwd() here...
#
# setwd(~/Whatever)
#
# Packages:

P<-c("readr","car","psych","stargazer","lmtest",
     "plotrix","dplyr","dotwhisker")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MULTIVARIATE REGRESSION...                    ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Toy regression example (N=4):

Y<-c(4,-2,9,-5)
X1<-c(200,120,430,110)
X2<-c(-17,32,-29,25)
data<-cbind(Y,X1,X2)
scatterplotMatrix(data)

cor(data)

fit<-lm(Y~X1+X2)
summary(fit)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Real" data examples...                       ####

Data<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2023-git/master/Data/africa2001.csv")
Data<-with(Data, data.frame(adrate,polity,
                            subsaharan=as.numeric(as.factor(subsaharan))-1,
                            muslperc,literacy))
summary(Data)
cor(Data)

# Scatterplot matrix:

scatterplotMatrix(Data)

# Linear model...

model<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
summary(model)

options(digits=4)
vcov(model)

# Added variable plot:

avmodel1<-lm(adrate~polity+subsaharan+muslperc,data=Data)
avmodel2<-lm(literacy~polity+subsaharan+muslperc,data=Data)

# Plot:

pdf("AddVarPlot.pdf",7,6)
par(mar=c(4,4,2,2))
plot(avmodel2$residuals,avmodel1$residuals,pch=20,
     xlab="Literacy | Model: Residuals", 
     ylab="HIV Rates | Model: Residuals")
abline(h=0,lwd=1,lty=2,col="grey")
abline(v=0,lwd=1,lty=2,col="grey")
abline(lm(avmodel1$residuals~avmodel2$residuals),lwd=3)
dev.off()

# Same thing, using avPlots from the -car- package:

avPlots(model,~literacy)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Linear hypothesis (F) tests...

modelsmall<-lm(adrate~muslperc+literacy,data=Data)
waldtest(model,modelsmall)  # from -lmtest- package

# Or:

linearHypothesis(model,"muslperc=0.1") # from -car-

linearHypothesis(model,"literacy=muslperc")

# Confidence ellipse

confidenceEllipse(model=model,which.coef=c(4,5),
                  xlab="Muslim Percentage",ylab="Literacy")
abline(h=0,v=0,lty=2)

# Predicted values:

hats<-fitted(model)

# Or, alternatively:

fitted<-predict(model,se.fit=TRUE, interval=c("confidence"))

# Plotted:

scatterplot(model$fitted~Data$adrate,log="x",smooth=FALSE,boxplots=FALSE,
            reg.line=FALSE,xlab="Observed HIV Rate",ylab="Predicted HIV Rate",
            pch=16,cex=2)

# Or:

plotCI(Data$adrate,model$fitted,uiw=(1.96*(fitted$se.fit)),
       log="x",xlab="Observed HIV Rate",ylab="Predicted HIV Rate")
lines(lowess(Data$adrate,Data$adrate),lwd=2)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Presentation...                               ####
#
# Re-fit three models:

M1<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
M2<-lm(adrate~polity+subsaharan+muslperc,data=Data)
M3<-lm(adrate~polity+subsaharan+literacy,data=Data)

# A (default) table:

stargazer(M1,M2,M3)

# A dot-whisker (or "ladder") plot of the un-rescaled
# coefficients:

pdf("ExampleDotWhisker.pdf",6,5)
dwplot(list(M1,M2,M3),
    vline=geom_vline(xintercept = 0,
              colour = "black",linetype = 2),
    vars_order=c("polity","subsaharan","muslperc","literacy")) %>%
    relabel_predictors(c(polity="POLITY",
                            subsaharan="Sub-Saharan",
                            muslperc="Muslim Percentage",
                            literacy="Literacy Rate")) +
        theme_classic() +
        xlab("Coefficient Estimate") +
        scale_color_hue(labels=c('Full Model','w/o Literacy','w/o Muslim Pct.'))
dev.off()        

# A rescaled-by-two-standard-deviations dot-whisker plot:

pdf("BetterDotWhisker.pdf",6,5)
dwplot(list(M1,M2,M3),by_2sd = TRUE, # <-- plot option
       vline=geom_vline(xintercept = 0,
                        colour = "black",linetype = 2),
       vars_order=c("polity","subsaharan","muslperc","literacy")) %>%
        relabel_predictors(c(polity="POLITY",
                             subsaharan="Sub-Saharan",
                             muslperc="Muslim Percentage",
                             literacy="Literacy Rate")) +
        theme_classic() +
        xlab("Coefficient Estimate") +
        scale_color_hue(labels=c('Full Model','w/o Literacy','w/o Muslim Pct.'))
        dev.off()  

# /fin