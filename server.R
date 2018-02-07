library(shiny)
library(lubridate)
library(quantmod)
#library(PerformanceAnalytics)
#library(quantstrat) # for backtest1
#library(knitr) # for backtest1
#library(blotter)

shinyServer(function(input, output) {
    getSpan <- reactive({
        input$symbol #need to rerun on new symbol
        toDate <<- NULL
        if (input$span == "1M"){
            fromDate <<- Sys.Date() %m-% months(1)
        }
        else if (input$span == "3M"){
            fromDate <<- Sys.Date() %m-% months(3)
        }
        else if (input$span == "6M"){
            fromDate <<- Sys.Date() %m-% months(6)
        }
        else if (input$span == "YTD"){
            fromDate = today()
            month(fromDate) = 1
            day(fromDate) = 1
        }
        else if (input$span == "1Y"){
            fromDate <<- Sys.Date() - years(1)
        }
        else if (input$span == "2Y"){
            fromDate <<- Sys.Date() - years(2)
        }
        else if (input$span == "5Y"){
            fromDate <<- Sys.Date() - years(5)
        }
        else if (input$span == "10Y"){
            fromDate <<- Sys.Date() - years(10)
        }
        else if (input$span == "MAX"){
            fromDate <<- gstart1
        }
        else{
            fromDate <<- input$dateRange[1]
            toDate   <<- input$dateRange[2]
        }
        if (fromDate < gstart1) fromDate <- gstart1
        if (is.null(fromDate)){
            strSpan <<- paste0(fromDate, "::")
        }
        else{
            strSpan <<- paste0(fromDate, "::", toDate)
        }
        strSpan
    })
    getSpan2 <- reactive({
        indx1 <- input$itrade
        indx2 <- input$itrade+input$ntrade
        if (indx1 > (NROW(kdata)-1)) indx1 <- (NROW(kdata)-1)
        if (indx2 > NROW(kdata)) indx2 <- NROW(kdata)
        date1 <- date(kdata[indx1])
        date2 <- date(kdata[indx2])
        strSpan2 <<- paste0(as.character(date1), "::", as.character(date2))
    })
    getData <- reactive({
        symbol  <<- input$symbol # can include leading "^"
        gsymbol <<- ifelse(substring(symbol,1,1) == "^", substring(symbol,2), symbol) # use for get
        #data = getSymbols(input$symbol, src = 'yahoo', from = '1980-01-01', auto.assign = F)
        getSymbols(symbol, src = 'yahoo', from='1900-01-01')
        #gdata same as mktdata
        gdata <<- get(gsymbol)
        gstart0 <<- date(gdata[1])
        gstart1 <<- date(gdata[input$ival1])
    })
    bt1data <- reactive({
        getData()
        strSpan <<- getSpan()
        if (input$adjusted){
            Close <- Ad(gdata)
        }
        else{
            Close <- Cl(gdata)
        }
        if (input$ilab1 == "SMA"){
            longMA <- SMA(Close, n = input$ival1)
        }
        else{
            longMA <- EMA(Close, n = input$ival1)
        }
        if (input$ilab2 == "SMA"){
            shrtMA <- SMA(Close, n = input$ival2)
        }
        else{
            shrtMA <- EMA(Close, n = input$ival2)
        }
        colnames(longMA) <- "longMA"
        colnames(shrtMA) <- "shrtMA"
        hdata <<- merge.xts(gdata, longMA, shrtMA)
        flag <- ifelse(hdata$shrtMA > hdata$longMA, 1, 0)
        flag[is.na(flag[,1]),] <- -1
        lag1 <- lag(flag, 1)
        lag2 <- lag(flag, 2)
        lag1[1] <- -1
        lag2[1:2] <- -1
        colnames(flag)  <- "flag"
        colnames(lag1) <- "lag1"
        colnames(lag2) <- "lag2"
        N <- 1:NROW(hdata)
        hdata <<- merge.xts(N, hdata, flag, lag1, lag2)
        #gdata <<- gdata[!is.na(gdata$flag)] # remove rows with no longMA
        hdata$lag1[hdata$lag1 < 0] <<- hdata$flag[hdata$lag1 < 0]
        hdata$lag2[hdata$lag2 < 0] <<- hdata$flag[hdata$lag2 < 0]
        #hdata <<- hdata[,c(1,4,7:11)] # just keep Open and Close
        idata <<- hdata[strSpan,]
        indx <<- as.numeric(which(idata$lag1 != idata$lag2))
        if (NROW(indx) == 0 | indx[1] != 1) indx <<- c(1, indx)
        if (indx[NROW(indx)] != NROW(idata)) indx <<- c(indx, NROW(idata))
        kdata <<- idata[indx,2:NCOL(idata)]
        if (input$trade == "open"){
            lastp <- lag(Op(kdata), 1)
            gain   <- Op(kdata) / lastp
        }
        else{
            if (input$adjusted){
                lastp <- lag(Ad(kdata), 1)
                gain   <- Ad(kdata) / lastp
                
            }
            else{
                lastp <- lag(Cl(kdata), 1)
                gain   <- Cl(kdata) / lastp
            }
        }
        colnames(lastp) <- "lastp"
        colnames(gain)   <- "gain"
        N <- 1:NROW(kdata)
        kdata <<- merge.xts(N, kdata, lastp, gain)
        ldata <<- kdata[kdata$lag2 == 1,]
        indx1 <- input$itrade
        indx2 <- input$itrade+input$ntrade
        if (indx1 > (NROW(kdata)-1)) indx1 <- (NROW(kdata)-1)
        if (indx2 > NROW(kdata)) indx2 <- NROW(kdata)
        mdata <<- kdata[indx1:indx2]
        mdata$lastp[1] <<- NA
        mdata$gain[1]  <<- NA
        ndata <<- mdata[mdata$lag2 == 1,]
    })
    output$bt1Plot1 <- renderPlot({
        bt1data()
        cat(file=stderr(), paste0(
            input$symbol,"|",input$type,"|",input$theme,"#",
            input$ilab1,"|", input$ival1,"|",input$icol1,"#",
            input$ilab2,"|", input$ival2,"|",input$icol2,"#",
            input$volume,"|",input$bollinger,"|",input$multicol,"|",input$logscale,"#",
            input$dateRange[1],"|",input$dateRange[2],"|",input$span,"#", #input$cols,"|",
            input$trade, "|",input$itrade,"|",input$ntrade,"\n"))
        indx1 <- input$itrade
        indx2 <- input$itrade+input$ntrade
        bt1Plot(strSpan, indx1, indx2)
    })
    output$bt1Plot2 <- renderPlot({
        bt1data()
        getSpan2()
        #print(paste0("strSpan2=",strSpan2)) #DEBUG
        bt1Plot(strSpan2, 0, 0)
    })
    bt1Plot <- function(span, indx1, indx2){
        idata <- hdata[span]
        # need to handle if NROW(idata) == 0
        indx <<- which(idata$lag1 != idata$lag2)
        if (NROW(indx) == 0 | indx[1] != 1) indx <<- c(1, indx)
        if (indx[NROW(indx)] != NROW(idata)) indx <<- c(indx, NROW(idata))
        #print(paste0("indx=",indx))
        #next line needed to avoid "improperly set or missing graphics device" error
        chartSeries(gdata, name = gsymbol, type = input$type, subset = strSpan, theme = input$theme, multi.col = input$multicol, log.scale = input$logscale)
        tas <- NULL
        if (input$volume == TRUE){
            tas = c(tas, addVo())
        }
        if (input$bollinger == TRUE){
            tas = c(tas, addBBands())
        }
        if (input$ilab1 == "SMA"){
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addSMA(n = num1, col = col1))
            }
        }
        else{
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addEMA(n = num1, col = col1))
            }
        }
        if (input$ilab2 == "SMA"){
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addSMA(n = num2, col = col2))
            }
        }
        else{
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addEMA(n = num2, col = col2))
            }
        }
        #cat(file = stderr(), paste0("chartSeries(", symbol, ", ", strSpan, ")\n"))
        if (is.null(tas)){
            chartSeries(gdata, name = gsymbol, type = input$type, TA = NULL, subset = span, theme = input$theme, multi.col = input$multicol, log.scale = input$logscale)
        }
        else{
            chartSeries(gdata, name = gsymbol, type = input$type, TA = tas, subset = span, theme = input$theme, multi.col = input$multicol, log.scale = input$logscale)
        }
        clrs <<- ifelse(as.numeric(idata$lag1[indx]) == 1, "blue", "red")
        #if (indx1 > 0) clrs[indx1] <- paste(clrs[indx1], " 4")
        #if (indx2 > 0) clrs[indx2] <- paste(clrs[indx2], " 4")
        if (indx1 > (NROW(clrs)-1)) indx1 <- (NROW(clrs)-1)
        if (indx1 > 0) clrs[indx1] <- "green"
        if (indx2 > NROW(clrs)) indx2 <- NROW(clrs)
        if (indx2 > 0) clrs[indx2] <- "green"
        addLines(v=indx, col=clrs)
    }
    output$bt1Gains <- renderPrint({
        bt1data()
        symbol <- input$symbol # can include leading "^"
        #print(paste0("symbol      =", symbol))
        #print(paste0("getSpan()   =", getSpan()))
        #print(paste0("start(gdata)=", start(gdata)))
        if (input$trade == "open") tlabel <- "Open"
        else tlabel <- "Close"
        kprod <- prod(kdata$gain, na.rm = TRUE)
        lprod <- prod(ldata$gain, na.rm = TRUE)
        mprod <- prod(mdata$gain, na.rm = TRUE)
        nprod <- prod(ndata$gain, na.rm = TRUE)
        cat(paste0("##### FIRST PLOT: ",start(kdata),"::",end(kdata)," #####\n"))
        #cat(paste0(tlabel, "(span)=", as.numeric(Cl(kdata)[NROW(kdata)]) / as.numeric(Cl(kdata)[1]), "\n"))
        cat(paste0(tlabel, "(all) =", kprod, "\n"))
        cat(paste0(tlabel, "(long)=", lprod, "\n"))
        if (kprod <= lprod){
            cat(paste0("GAIN = ",100*(lprod-kprod)/kprod," percent\n"))
        }
        else{
            cat(paste0("LOSS = ",100*(lprod-kprod)/kprod," percent\n"))
        }
        cat(paste0("##### SECOND PLOT: ",start(mdata),"::",end(mdata)," #####\n"))
        cat(paste0(tlabel, "(all) =", mprod, "\n"))
        cat(paste0(tlabel, "(long)=", nprod, "\n"))
        if (mprod <= nprod){
            cat(paste0("GAIN = ",100*(nprod-mprod)/mprod," percent\n"))
        }
        else{
            cat(paste0("LOSS = ",100*(nprod-mprod)/mprod," percent\n"))
        }
    })
    output$bt1Trades <- renderPrint({
        bt1data()
        cat("FIRST PLOT (all)\n")
        print(kdata[,c(1,as.numeric(input$cols),8:14)])
        cat("\nFIRST PLOT (long)\n")
        print(ldata[,c(1,as.numeric(input$cols),8:14)])
        cat("\nSECOND PLOT (all)\n")
        print(mdata[,c(1,as.numeric(input$cols),8:14)])
        cat("\nSECOND PLOT (long)\n")
        print(ndata[,c(1,as.numeric(input$cols),8:14)])
        cat("\nSECOND PLOT (all data)\n")
        getSpan2()
        idata <- hdata[strSpan2,c(1,as.numeric(input$cols),8:12)]
        print(idata)
        if ((NROW(idata) * NCOL(idata)) > options("max.print")){
            cat("...\n")
            print(tail(idata))
        }
    })
    output$bt1Data <- renderPrint({
        bt1data()
        cat(paste0(input$symbol,"\n"))
        print(head(gdata))
        print(tail(gdata))
        print(paste0("gstart0=",gstart0))
        print(paste0("gstart1=",gstart1))
    })
})