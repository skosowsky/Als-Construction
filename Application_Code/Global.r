library(shiny)
library(stats)
library(chron)
library(RCurl)
library(pROC)
library(plotly)
library(dplyr)
Sys.setenv("plotly_username"="skosowsky")
Sys.setenv("plotly_api_key"="h7fhkzhaia")
options(warn=-1)
na.action="na.exclude"
url<-getURL("https://raw.githubusercontent.com/skosowsky/Als-Construction/master/Construction%20Data_Train.csv")
Construction.Data_Train<-read.csv(text=url)
Construction.Data_Train<-na.omit(Construction.Data_Train)
attach(Construction.Data_Train)
Years_Bin<-vector(length=433)
Years_Bin[which(Estimated.Years.to.Complete<=1)]<-"Quick"
Years_Bin[which(Estimated.Years.to.Complete>1 & Estimated.Years.to.Complete<5)]<-"Medium"
Years_Bin[which(Estimated.Years.to.Complete>=5)]<-"Long"

Cost_Bin<-vector(length=433)
Cost_Bin[which(Estimated.Cost..Millions<=100)]<-"Low"
Cost_Bin[which(Estimated.Cost..Millions>100 & Estimated.Cost..Millions<299)]<-"Medium"
Cost_Bin[which(Estimated.Cost..Millions>=299)]<-"High"

Profit<- ((Bid.Price..Millions- Estimated.Cost..Millions)/Estimated.Cost..Millions)*100

log5<-glm(as.factor(Win.Bid) ~Cost_Bin+Years_Bin+as.factor(Sector)+ as.factor(Region.of.Country) + 
            + Profit +Competitor.E + Competitor.F + 
            Competitor.H+Competitor.I+ Competitor.J, family = binomial)

full_vars<-as.data.frame(cbind(Construction.Data_Train$Competitor.H,Years_Bin,Cost_Bin))
step<-lm(Estimated.Cost..Millions ~ 
           Competitor.H + Years_Bin*Cost_Bin)

pred5<- predict.glm(log5, type="response") 

ROC5<-roc(log5$y,log5$fitted.values)
class_table5<-as.data.frame(cbind(ROC5$thresholds,ROC5$sensitivities,ROC5$specificities))
class_table5$V1[which.max(class_table5$V2[-1]+class_table5$V3[-1])]

Pred_True<-vector(length=433)
Pred_True[which(pred5>=class_table5$V1[which.max(class_table5$V2[-1]+class_table5$V3[-1])])]="Pred=T" 
Pred_True[which(pred5<class_table5$V1[which.max(class_table5$V2[-1]+class_table5$V3[-1])])]="Pred=F" 
table(Win.Bid,Pred_True)
Wrong_Pred<-Construction.Data_Train[which(Win.Bid=="Yes" & Pred_True=="Pred=F") ,]