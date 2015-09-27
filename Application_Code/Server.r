shinyServer(function(input, output,session) {
detach(Construction.Data_Train)
  updateSelectizeInput(session,'Competitors', 
    choices = list(
      'Competitor A'='Competitor.A', 'Competitor B'='Competitor.B','Competitor C'='Competitor.C',
      'Competitor D'='Competitor.D','Competitor E'='Competitor.E',
      'Competitor F'='Competitor.F','Competitor G'='Competitor.G',
      'Competitor H'='Competitor.H','Competitor I'='Competitor.I','Competitor J'='Competitor.J'))
  
  
 
   updateSelectizeInput(session,'SectorInput', choices=list(
     'Transportation'=1,'Lodging'=2,'Multi-Family Residential'=3,'Amusement and Recreation'=4,
     'Highway and Street'=5,'Education'=6,'Healthcare'=7,'Manufacturing'=8,'Power'=9,'Military'=10)
      )
   
   updateSelectizeInput(session,'CostInput', choices=list(
     'Under $100,000,000'='Low','$100,000,000-$300,000,000'='Medium','Over $300,000,000'='High'
     )
   )
   
   updateSelectizeInput(session,'LengthInput', choices=list(
     'Shorter than 1 Year'='Quick','1-5 Years'='Medium','Longer than 5 Years'='Long'
   )
   )

   
Make_Data<-function(cost,profit,years,sector,region,competitors){   
  Cost<-input$CostInput
  Years<-input$LengthInput
  Sector<-input$SectorInput
  Region<-input$RegiontInput
  Competitors<-c(0,input$Competitors)
  Shiny_Set<-as.data.frame(matrix(nrow=200,ncol=22))
  colnames(Shiny_Set)<-c("Years_Bin","Cost_Bin",
                         "Profit","Estimated.Cost..Millions",
                         "Estimated.Years.to.Complete","Bid.Price..Millions",
                         "Sector","Region.of.Country",
                         "Number.of.Competitor.Bids","Competitor.A",
                         "Competitor.B","Competitor.C",
                         "Competitor.D","Competitor.E",
                         "Competitor.F","Competitor.G",
                         "Competitor.H","Competitor.I",
                         "Competitor.J","Win.Bid",
                         "Winning.Bid.Price..Millions","Cost.After.Engineering.Estimate..Thousands")
  Shiny_Set$Profit=as.numeric(seq(0,.4,.4/199)*100)
  Shiny_Set$Cost_Bin<-as.factor(Cost)
  Shiny_Set$Years_Bin<-Years
  Shiny_Set$Sector<-Sector
  Shiny_Set$Region.of.Country<-Region
  if("Competitor.A" %in% Competitors){Shiny_Set$Competitor.A=1}
  else{Shiny_Set$Competitor.A=0}
  if("Competitor.B" %in% Competitors){Shiny_Set$Competitor.B=1}
  else{Shiny_Set$Competitor.B=0}
  if("Competitor.C" %in% Competitors){Shiny_Set$Competitor.C=1}
  else{Shiny_Set$Competitor.C=0}
  if("Competitor.D" %in% Competitors){Shiny_Set$Competitor.D=1}
  else{Shiny_Set$Competitor.D=0}
  if("Competitor.E" %in% Competitors){Shiny_Set$Competitor.E=1}
  else{Shiny_Set$Competitor.E=0}
  if("Competitor.F" %in% Competitors){Shiny_Set$Competitor.F=1}
  else{Shiny_Set$Competitor.F=0}
  if("Competitor.G" %in% Competitors){Shiny_Set$Competitor.G=1}
  else{Shiny_Set$Competitor.G=0}
  if("Competitor.H" %in% Competitors){Shiny_Set$Competitor.H=1}
  else{Shiny_Set$Competitor.H=0}
  if("Competitor.I" %in% Competitors){Shiny_Set$Competitor.I=1}
  else{Shiny_Set$Competitor.I=0}
  if("Competitor.J" %in% Competitors){Shiny_Set$Competitor.J=1}
  else{Shiny_Set$Competitor.J=0}
  Shiny_Set$Estimated.Cost..Millions=0
  Shiny_Set$Estimated.Years.to.Complete=0
  Shiny_Set$Bid.Price..Millions=0
  Shiny_Set$Number.of.Competitor.Bids=0
  Shiny_Set$Win.Bid=0
  Shiny_Set$Winning.Bid.Price..Millions=0
  Shiny_Set$Cost.After.Engineering.Estimate..Thousands=0
  Shiny_Set
}

make_output_data<-function(cost,profit,years,sector,region,competitors,nrows){
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  m<-as.data.frame(cbind(pred,m,as.factor(est_win)))
if(nrows==10){
  Index<-seq(1,200,20)}
else if(nrows==20){
    Index<-seq(1,200,10)}
else if(nrows==30){
  Index<-seq(1,200,6.7)}
else if(nrows==40){
  Index<-seq(1,200,5)}
else if(nrows==50){
  Index<-seq(1,200,4)}
else if(nrows==60){
  Index<-seq(1,200,3.35)}
else if(nrows==70){
  Index<-seq(1,200,2.85)}
else if(nrows==80){
  Index<-seq(1,200,2.5)}
else if(nrows==90){
  Index<-seq(1,200,2.25)}
else if(nrows==100){
  Index<-seq(1,200,2)}

  outdata<-as.data.frame(cbind(round(pred,4)*100,round(profit,4)*100,round(profit*pred,4)*100,est_win),row.names=NULL)[Index,]
  row.names(outdata)<-NULL
  colnames(outdata)<-c("Probability of Winning Bid (%)","Profit Margin (%)","Probability-Weighted Profit Margin (%)","Recommendation to Bid")
  outdata
}



make_max_data<-function(cost,profit,years,sector,region,competitors){
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  m<-as.data.frame(cbind(pred,m,as.factor(est_win)))
  expprofit<-profit*pred
  outmaxdata<-as.data.frame(cbind(round(pred[which.max(expprofit)],4)*100,round(profit[which.max(expprofit)],4),round(expprofit[which.max(expprofit)],4),est_win),row.names=NULL)
  row.names(outmaxdata)<-NULL
  colnames(outmaxdata)<-c("Probability of Winning Bid (%)","Profit Margin (%)","Probability-Weighted Profit Margin (%)","Recommendation to Bid")
  outmaxdata[1,]
}


prof_plot_2d<-function(cost,profit,years,sector,region,competitors){
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  m<-as.data.frame(cbind(pred,m,as.factor(est_win)))
  x <- list(
    title = "Profit Margin (%)"
  )
  y <- list(
    title = "Probability-Weighted Profit Margin (%)"
  )
  plot_ly(m,x=profit,y=pred*profit,type="scatter",color=est_win,colors=c("lightblue3","orange"),mode="markers",filename="scatter") %>%
  layout(xaxis=x,yaxis=y,title="Profit Margin vs Probability-Weighted Profit Margin")
    }

prob_plot_2d<-function(cost,profit,years,sector,region,competitors){
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  m<-as.data.frame(cbind(pred,m,as.factor(est_win)))
  x <- list(
    title = "Predicted Probability of Winning Bid (%)"
  )
  y <- list(
    title = "Probability-Weighted Profit Margin (%)"
  )
  plot_ly(m,x=pred*100,y=pred*profit,type="scatter",color=est_win,colors=c("lightblue3","orange"),mode="markers",filename="scatter")%>%
    layout(xaxis=x,yaxis=y,title="Probability of Winning Bid vs Probability-Weighted Profit Margin")
}

prob_prof_plot_2d<-function(cost,profit,years,sector,region,competitors){
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  m<-as.data.frame(cbind(pred,m,as.factor(est_win)))
  x <- list(
    title = "Predicted Probability of Winning Bid (%)"
  )
  y <- list(
    title = "Profit Margin (%)"
  )
  plot_ly(m,x=pred*100,y=profit,type="scatter",color=est_win,colors=c("lightblue3","orange"),mode="markers",filename="scatter")%>%
    layout(xaxis=x,yaxis=y, title= "Predicted Probability of Winning vs Profit Margin")
}

Make_Linear_Data<-function(cost,years,competitors){   
  Cost<-input$CostInput
  Years<-input$LengthInput
  Sector<-input$SectorInput
  Region<-input$RegiontInput
  Competitors<-c(0,input$Competitors)
  Shiny_Set<-as.data.frame(matrix(nrow=200,ncol=22))
  colnames(Shiny_Set)<-c("Years_Bin","Cost_Bin",
                        "Competitor.H")
  Shiny_Set$Cost_Bin<-as.factor(Cost)
  Shiny_Set$Years_Bin<-Years
    if("Competitor.H" %in% Competitors){Shiny_Set$Competitor.H=1}
  else{Shiny_Set$Competitor.H=0}
    Shiny_Set
}

predict_cost<-function(years,cost,competitors){
  m<-Make_Linear_Data(cost,years,competitors)
  pred_cost<-predict(step,newdata=m)
  pred_cost
}

make_bid<-function(years,cost,profit,competitors){
  cost=predict_cost(years,cost,competitors)
  bidlev=profit*(.01)*cost+cost
  bidlev
}
make_top_table<-function(years,cost,sector,competitors,profit){
  p_cost<-predict_cost(years,cost,competitors)
  m<-Make_Data(cost,profit,years,sector,region,competitors)
  pred<-predict.glm(log5,new=m,type="response")
  expprofit<-seq(0,.4,.4/199)*100*pred
  est_win<-vector(length=200)
  est_win[which(pred>=0.1781271)]="Bid"
  est_win[which(pred<0.1781271)]="Do Not Bid"
  bidlevel<-make_bid(years,cost,profit,competitors)
  if(est_win[which.max(expprofit)]=="Bid"){
  out2<-as.data.frame(cbind(p_cost,bidlevel))[which.max(expprofit),]}
  else{out2<-as.data.frame(cbind(p_cost,"N/A"))[which.max(expprofit),]}  
  colnames(out2)<-c("Estimated Cost (Millions)","Bid Price (Millions)")
  out2
}

output$data_max<- renderTable({make_max_data(input$CostInput,as.numeric(seq(0,.4,.4/199)*100),input$LengthInput,input$SectorInput,input$RegiontInput,input$Competitors)})
output$bottom_data_table<- renderTable({make_output_data(input$CostInput,as.numeric(seq(0,.4,.4/199)),input$LengthInput,input$SectorInput,input$RegiontInput,input$Competitors,input$Numrows)})
output$top_data_table<- renderTable({make_top_table(as.factor(input$LengthInput),as.factor(input$CostInput),as.factor(input$SectorInput),input$Competitors,as.numeric(seq(0,.4,.4/199)*100))})
output$Prof_Plot<- renderPlotly({prof_plot_2d(input$CostInput,as.numeric(seq(0,.4,.4/199)*100),input$LengthInput,input$SectorInput,input$RegiontInput,input$Competitors)})
output$Prob_Plot<- renderPlotly({prob_plot_2d(input$CostInput,as.numeric(seq(0,.4,.4/199)*100),input$LengthInput,input$SectorInput,input$RegiontInput,input$Competitors)})
output$Prob_Prof_Plot<- renderPlotly({prob_prof_plot_2d(input$CostInput,as.numeric(seq(0,.4,.4/199)*100),input$LengthInput,input$SectorInput,input$RegiontInput,input$Competitors)})
output$Test<-renderPlot({predict_cost(input$LengthInput,input$CostInput,input$Competitors)})
})


