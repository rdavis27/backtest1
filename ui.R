library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Backtest"),
    
    # Sidebar with a slider input for the number of bins
    fluidRow(
        column(4, wellPanel(
            fluidRow(
                column(4, style='padding:1px', textInput("symbol", "Symbol", value = "SPY")),
                column(4, style='padding:1px', selectInput("type", "Type", c("auto","candlesticks","matchsticks","bars","line"), selected = "auto")),
                column(4, style='padding:1px', selectInput("theme", "Theme", c("white","black"), selected = "white"))
            ),
            fluidRow(
                column(2, style='padding:1px', selectInput("ilab1", "High ind", c("none","EMA","SMA"), selected = "SMA", width = 80)),
                column(2, style='padding:1px', selectInput("icol1", "color", c("blue","red","green","purple","orange"), selected = "blue", width = 80)),
                column(2, style='padding:1px', numericInput("ival1", "value", 200, width = 80)),
                column(2, style='padding:1px', numericInput("imin1", "min", 10, width = 80)),
                column(2, style='padding:1px', numericInput("imax1", "max", 200, width = 80)),
                column(2, style='padding:1px', numericInput("istp1", "step", 10, width = 80))
            ),
            fluidRow(
                column(2, style='padding:1px', selectInput("ilab2", "Low ind", c("none","EMA","SMA"), selected = "SMA", width = 80)),
                column(2, style='padding:1px', selectInput("icol2", "color", c("blue","red","green","purple","orange"), selected = "red", width = 80)),
                column(2, style='padding:1px', numericInput("ival2", "value", 50, width = 80)),
                column(2, style='padding:1px', numericInput("imin2", "min", 10, width = 80)),
                column(2, style='padding:1px', numericInput("imax2", "max", 200, width = 80)),
                column(2, style='padding:1px', numericInput("istp2", "step", 10, width = 80))
            ),
            selectInput("calc", "Calculate", c("Use values","Vary high indicator","Vary low indicator","Vary both indicators"), selected = "Use values"),
            fluidRow(
                column(4, style='padding:1px', checkboxInput("adjusted", "Adjusted", TRUE)),
                column(4, style='padding:1px', checkboxInput("volume", "Volume", TRUE)),
                column(4, style='padding:1px', checkboxInput("logscale", "Log Scale", FALSE)),
                column(4, style='padding:1px', checkboxInput("bollinger", "Bollinger Bands", FALSE)),
                column(8, style='padding:1px', checkboxInput("multicol", "4-colored Candles", FALSE))
            ),
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = '2010-01-01', end = Sys.Date()),
            radioButtons("span", "Timespan:", inline = TRUE,
                         c("Use above dates","1M","3M","6M","YTD","1Y","2Y","5Y","10Y","MAX"), selected = "MAX"),
            checkboxGroupInput("cols","Columns to include:", inline = TRUE,
                               choices=c("Open"=2,"High"=3,"Low"=4,"Close"=5,"Volume"=6,"Adjusted"=7),
                               selected=c(2, 5)),
            fluidRow(
                column(4, style='padding:1px', selectInput("trade", "Trade at", c("open","close"), selected = "close")),
                column(4, style='padding:1px', numericInput("itrade", "First trade to plot", -1)),
                column(4, style='padding:1px', numericInput("ntrade", "# of trades to plot", 1, min = 1))
            )
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
               ),
               tabPanel(
                   "Scan",
                   verbatimTextOutput("bt1Scan")
               )
            )
        )
    )
))