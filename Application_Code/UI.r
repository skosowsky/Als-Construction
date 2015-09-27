shinyUI(pageWithSidebar(
  headerPanel(
    if(times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))>"6:00:00"& times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))<"12:00:00"){
      "Good Morning, Al"
    }
    else if(times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))>"12:00:00"&times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))<"18:00:00"){
      "Good Afternoon, Al"
    }
    else if(times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))>"18:00:00"&times(strftime(Sys.time(),"%H:%M:%S",tz="EST5EDT"))<"20:00:00"){
      "Good Evening, Al"
    }
    else{"Go to Sleep, Al"}
  ),
  sidebarPanel(
        selectizeInput('Competitors', 'Select Competing Bidders:', choices = list(
      'Competitor A', 'Competitor B','Competitor C','Competitor D','Competitor E',
      'Competitor F','Competitor G','Competitor H','Competitor I','Competitor J'), multiple = TRUE,
      options = list(placeholder = 'Select Competing Bidders',
      onInitialize = I('function() { this.setValue(""); }'))),
    
    selectizeInput('LengthInput','Estimated Length of Project:', choices=list(
      'Shorter than 1 Year','1-5 Years','Longer than 5 Years'),
      options = list(placeholder = 'Select an Estimated Length',
      onInitialize = I('function() { this.setValue(""); }'))),
    
    selectizeInput('CostInput','Estimated Cost of Project:', choices=list(
      'Under $100,000,000','$100,000,000-$300,000,000','Over $300,000,000'),
      options = list(placeholder = 'Select an Estimated Cost',
                     onInitialize = I('function() { this.setValue(""); }'))),

#conditionalPanel(
#  condition = "input.CostInput == 'Enter a Number:'",
#  textInput('CustomCost', "Enter a Cost:")
#)
#    ,
    
    selectizeInput('RegiontInput','Select a Region:', choices=list(
      'Northeast','Southeast','Southwest','Mid-west','West'),
      options = list(placeholder = 'Select a Region',
      onInitialize = I('function() { this.setValue(""); }'))),
    
    selectizeInput('SectorInput','Select a Sector:', choices=list(
      'Transportation','Lodging','Multi-Family Residential','Amusement and Recreation',
      'Highway and Street','Education','Healthcare','Manufacturing','Power','Military'),
      options = list(placeholder = 'Select a Sector',
      onInitialize = I('function() { this.setValue(""); }'))),

submitButton("Submit"),

conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
  h4("Recommended Bid Price to Maximize Profit")),

conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
   tableOutput("top_data_table")),

conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
  tableOutput('data_max')
),
conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
  h4("Detailed Bid Information")),

conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
  selectizeInput('Numrows',label="Number of Rows to Show",choices=list(
    10,20,30,40,50,60,70,80,90,100),selected=10)
),
conditionalPanel(
  condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
  tableOutput('bottom_data_table'))
),
  mainPanel(
         conditionalPanel(
     condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
     plotlyOutput('Prof_Plot')
   ),
   conditionalPanel(
      condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
     plotlyOutput('Prob_Plot')
   ),
   conditionalPanel(
     condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
     plotlyOutput('Prob_Prof_Plot')
   )
   ,
   conditionalPanel(
     condition = "input.CostInput == 'Low'|input.CostInput == 'Medium'|input.CostInput == 'High'",
     plotOutput('Test')
   )
   
  )
  
))