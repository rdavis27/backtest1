library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Backtest"),
    
    # Sidebar with a slider input for the number of bins
    fluidRow(
        column(4, wellPanel(
            textInput("symbol", "Symbol", value = "SPY"),
            selectInput("type", "Type", c("auto","candlesticks","matchsticks","bars","line"), selected = "auto"),
            selectInput("theme", "Theme", c("white","black"), selected = "white"),
            fluidRow(
                column(4, style='padding:1px', selectInput("ilab1", "Overlay 1", c("none","EMA","SMA"), selected = "SMA", width = 80)),
                column(4, style='padding:1px', numericInput("ival1", "value", 200, width = 80)),
                column(4, style='padding:1px', selectInput("icol1", "color", c("blue","red","green","purple","orange"), selected = "blue", width = 80))
            ),
            fluidRow(
                column(4, style='padding:1px', selectInput("ilab2", "Overlay 2", c("none","EMA","SMA"), selected = "SMA", width = 80)),
                column(4, style='padding:1px', numericInput("ival2", "value", 50, width = 80)),
                column(4, style='padding:1px', selectInput("icol2", "color", c("blue","red","green","purple","orange"), selected = "red", width = 80))
            ),
            checkboxInput("volume", "Volume", TRUE),
            checkboxInput("bollinger", "Bollinger Bands", FALSE),
            checkboxInput("multicol", "4-colored Candles", FALSE),
            checkboxInput("logscale", "Log Scale", FALSE),
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = '2010-01-01', end = Sys.Date()),
            radioButtons("span", "Timespan:", inline = TRUE,
                         c("Use above dates","1M","3M","6M","YTD","1Y","2Y","5Y","10Y","MAX"), selected = "MAX"),
            checkboxGroupInput("cols","Columns to include:", inline = TRUE,
                               choices=c("Open"=2,"High"=3,"Low"=4,"Close"=5,"Volume"=6,"Adjusted"=7),
                               selected=c(2, 5)),
            selectInput("trade", "Trade at", c("open","close"), selected = "close"),
            numericInput("itrade", "First trade to plot", 1, min = 1),
            numericInput("ntrade", "Number of trades to plot", 1, min = 1)
        )),
        # Show a plot of the generated distribution
        column(8,
            tabsetPanel(
               tabPanel(
                   "Charts",
                   plotOutput("bt1Plot1"),
                   plotOutput("bt1Plot2"),
                   verbatimTextOutput("bt1Gains")
               ),
               tabPanel(
                   "Trades",
                   verbatimTextOutput("bt1Trades")
               ),
               tabPanel(
                   "Data",
                   verbatimTextOutput("bt1Data")
               )
            )
        )
    )
))