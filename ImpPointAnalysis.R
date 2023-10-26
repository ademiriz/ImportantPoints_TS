# install.packages('curl', repos = 'http://cran.r-project.org')
# devtools::install_github('systematicinvestor/SIT.date')
# library(curl)
# curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
# install.packages('sit', repos = NULL, type='source')
# library(SIT)
# # Run plota test
# plota.test()

library(DT)
library(SIT)
library(datamods)
library(datasets)
library(dplyr)
library(dygraphs)
library(lubridate)
library(mlbench)
library(plotly)
library(quantmod)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(tidyquant)
library(tidyverse)
library(xts)

tickers <- c("AAPL", "GOOG", "THYAO.IS", "META")

prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = today()-months(48),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,open,high,low,close)

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    checkboxInput(var, var, value = TRUE)
  }  else {
    # Not supported
    NULL
  }
}


ui <-dashboardPage(
  title='Interactive Time Series',
  dashboardHeader(
    title = 'Time Series'
  ),
  dashboardSidebar(
                # Let user pick stocks
                 pickerInput(
                   inputId = "stocks",
                   label = h4("Stocks"),
                   choices = c(
                     "APPLE"       = tickers[1], 
                     "Google"   = tickers[2],
                     "THY"      = tickers[3],
                     "Meta"         = tickers[4]),
                   selected = tickers[3],   
                   options = list(`actions-box` = TRUE), 
                   multiple = F
                 ),
                 # Pick time period
                 numericInput("period", label = h4("Period (Month)"), 
                              value = 12, min = 1, max = 48, step = 1),
                 
                 hr(),
                 div(strong("From: "), textOutput("from", inline = TRUE)),
                 div(strong("To: "), textOutput("to", inline = TRUE))
                 
    
    
  ),
  dashboardBody(
    useShinyjs(),
    tabsetPanel(
      id = "navbar",
      tabPanel(title="Data",id="tab1",value='tab1_val',
               column(
                 width = 8,
                 tags$b("Imported data:"),
                 verbatimTextOutput(outputId = "name"),
                 DT::dataTableOutput('data')
               )),
      tabPanel(title="Important Points",id="tab3",value='tab3_val',
               column(2,
                      conditionalPanel(condition="output.dataLoaded",

                                       # uiOutput("filter"),
                                       selectInput(
                                         "variable",
                                         label = h5("Select Time Series/Response Variable"),multiple = TRUE,
                                         ""
                                       ),
                                       actionButton("goButton", "Update Data"),
                                       actionButton("resetButton", "Reset Data")


                      ),
                      conditionalPanel(condition="output.singleSeries",


                                       sliderInput("RValue", "R Value", 1.01, 1.5,
                                                   value = 1.2, step = 0.01
                                       ),
                                       actionButton("impPointButton", "Important Points")


                      )
               ),
               column(
                 width = 8,
                 wellPanel(dygraphOutput("Sensorgraph")
                 )
               )),

      tabPanel(title="Time Series Search",id="tab2",value='tab2_val',
               column(2,
                      conditionalPanel(condition="output.dataLoaded",

                                       # uiOutput("filter"),
                                       selectInput(
                                         "avariable",
                                         label = h5("Select Time Series/Response Variable"),multiple = FALSE,
                                         ""
                                       ),
                                       sliderInput("LastPoints", "Search Points", 5, 45,
                                                   value = 20, step = 5
                                       ),
                                       actionButton("goButtona", "Select Series")


                      )
                      ),
               column(
                 width = 8,
                 # DT::dataTableOutput('DTclustData')
                 wellPanel(plotOutput("dynamicTime") )
               )

      )


      
      
    )
  )
)



server <- function(input, output, session){
   
  observeEvent(c(input$period,input$stocks), {
    imported <- prices %>%
      filter(symbol %in% input$stocks)
    imported <- imported %>%
      filter(
        date >= today()-months(input$period)) 
    output$data = DT::renderDataTable(req(imported), server = TRUE)
    tbdata<-reactive({req(imported)})
    output$dataLoaded<-reactive(!is.null(imported))
    updateTextInput(session, "variable", value = names(dplyr::select_if(req(imported),is.numeric)))
    outputOptions(output, 'dataLoaded', suspendWhenHidden = FALSE)
    observe({
      updateSelectInput(
        session,
        "variable",
        #choices=c('open','high','low','close'))
        choices=names(dplyr::select_if(req(imported),is.numeric)))
      
    })
    observe({
      updateSelectInput(
        session,
        "avariable",
        choices=names(dplyr::select_if(req(imported),is.numeric)))
      
    })
   
    
  observeEvent(input$resetButton, {
    updateTextInput(session, "variable", value = names(dplyr::select_if(req(imported),is.numeric)))
    output$Sensorgraph <- renderDygraph({
      myxts<-xts( x=imported[,-1], order.by=imported$date)
      myxts%>%dygraph( main = "Stock Data") %>%
      dyOptions(drawGrid = input$showgrid) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(input$goButton, {
    myfrom<-req(input$Sensorgraph_date_window[[1]])
    myto<-req(input$Sensorgraph_date_window[[2]])
    output$from <- renderText({
      strftime(req(input$Sensorgraph_date_window[[1]]), "%d %b %Y")      
    })
    
    output$to <- renderText({
      strftime(req(input$Sensorgraph_date_window[[2]]), "%d %b %Y")
    })
    selected <- reactive({
      imported[imported$date>=myfrom & imported$date<=myto,c(input$variable,"date")]
    })
   
    output$singleSeries<-reactive(length(input$variable)==1)
    outputOptions(output, 'singleSeries', suspendWhenHidden = FALSE)
    
   
    output$Sensorgraph <- renderDygraph({
      myxts<-xts( x=selected()[,c(input$variable)], order.by=selected()$date)
      myxts%>%dygraph( main = "Stock Data") %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyRangeSelector()
    })
    
  })
  
  observeEvent(input$goButtona, {
    
    selecteda <- reactive({
      tbdata()[,c(input$avariable,"date")]
    })
    output$dynamicTime <- renderPlot({
      
      
      queryLastPoints<-req(input$LastPoints)
      xtsfile <- xts(x=selecteda()[,c(input$avariable)], order.by=selecteda()$date)
      data<-to.period(xtsfile[,1],period="hours" ) 
      tickers = 'Stock Price'
      #*****************************************************************
      # Setup search
      #****************************************************************** 
      
      reference <- coredata(Cl(data))
      n<-length(reference)
      query<-reference[(n-queryLastPoints+1):n]   
      reference<-reference[1:(n-queryLastPoints)]
      
      n.query<-length(query)
      n.reference<-length(reference)
    
      #*****************************************************************
      # Compute Distances
      #******************************************************************         
      dist<-rep(NA, n.reference)
      query.normalized <- (query - mean(query)) / sd(query)
      
      for( i in n.query : n.reference ) {
        window <- reference[ (i - n.query + 1) : i]
        window.normalized <- (window - mean(window)) / sd(window)
        dist[i] <- stats:::dist(rbind(query.normalized, window.normalized))
      }
      
      min.index <- c()
      n.match <- 10
      
      # only look at the minimums 
      temp <- dist
      temp[ temp > mean(dist, na.rm=T) ] <- NA
      
      # remove n.query, points to the left/right of the minimums
      for(i in 1:n.match) {
        if(any(!is.na(temp))) {
          index <- which.min(temp)
          min.index[i] <- index
          temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] <- NA
        }
      }
      n.match <- length(min.index)
      dates <- index(data)[1:length(dist)]
      
    
      
      plota(data, type='l', col='gray', main=tickers)
      plota.lines(xts::last(data,queryLastPoints), col='blue')
      for(i in 1:n.match) {
        plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
      }
      text(index(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match,
           adj=c(1,-1), col='black',xpd=TRUE)
      plota.legend('Pattern,Match #','blue,red')
      
      
    }, res = 96)
    
  })
  observeEvent(input$impPointButton, {
    myfrom<-req(input$Sensorgraph_date_window[[1]])
    myto<-req(input$Sensorgraph_date_window[[2]])
    output$from <- renderText({
      strftime(req(input$Sensorgraph_date_window[[1]]), "%d %b %Y")      
    })
    
    output$to <- renderText({
      strftime(req(input$Sensorgraph_date_window[[2]]), "%d %b %Y")
    })
    selected <- reactive({
      imported[imported$date>=myfrom & imported$date<=myto,c(input$variable,"date")]
    })

    a<-as.matrix(selected()[,1],ncol=1)
 
    n<-length(a)
    R<-req(input$RValue)
    b <- data.frame(val=double(),
                    ind=double())
    
    find_first_two <- function(){
      iMin <- 1
      iMax <- 1
      i<-1
  
      while (i<n & (a[i]/a[iMin] < R) & (a[iMax] /a[i] < R))
      {  
        if (a[i] < a[iMin])  {iMin = i}
        if (a[i] > a[iMax]) {iMax = i}
        i <- i + 1
      }
    
      if (iMin < iMax)
      {val<-c(a[iMin],a[iMax])
      ind<- c(iMin,iMax) }
      else {val<-c(a[iMax],a[iMin])
      ind<- c(iMax,iMin) }
      localb<-data.frame(val,ind)
  
      b<<-rbind(b,localb)

      return (i)
    }
    
    find_minimum<- function(i) {
      iMin = i
      while (i<n & a[i]/a[iMin] < R )
      {if (a[i] < a[iMin])
      { iMin <- i}
        i <- i + 1
      }
      b<<-rbind(b,c(a[iMin],iMin))
      return (i) 
    }
    
    
    find_maximum<- function(i) {
      iMax = i
      while (i<n & a[iMax] /a[i] < R )
      {if (a[i] > a[iMax])
      { iMax <- i}
        i <- i + 1
      }
      b<<-rbind(b,c(a[iMax],iMax))
      return (i) 
    }
    
 
    i <- find_first_two()

    if (i<n & a[i] > a[1]) 
    {i <- find_minimum(i)}
    while (i<n) {
      i <- find_maximum(i)
      i <- find_minimum(i)
    }
    
    tmp <- selected()[b$ind,c(1,2)]  # OHLC data only
    junkxts<-xts( x=tmp[,1], order.by=tmp$date)
    
    output$Sensorgraph <- renderDygraph({
      myxts<-xts( x=selected()[,1], order.by=selected()$date)
      
      i<-seq(1:length(junkxts))
      my_code<-paste("myxts%>%dygraph( ) %>%  dyOptions(drawGrid = input$showgrid) %>%dyRangeSelector()%>% ",
                     paste0("dyAnnotation(x=index(junkxts[",i,",]),text='X')",collapse = " %>% "))
      
      eval(parse(text = my_code))
    })
    
  }) 
  
  
  
 
  output$Sensorgraph <- renderDygraph({
    myxts<-xts( x=imported[,-1], order.by=imported$date)
    myxts%>%dygraph( main = "Stock Data") %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector()
  })
  
  
  
  })
  
   output$name <- renderPrint({
     head(input$stocks)
    
   })
  

  
}

shinyApp(ui, server)
