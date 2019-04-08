library(PerformanceAnalytics)
library(xts)
library(zoo)
library(ggplot2)
library(tidyr)
library(dplyr)

#easy test & package validation
data <- read.csv('expandData.csv')
data <- xts(data[1:48,-1], order.by = as.Date(as.character(data[1:48,1]), format = "%m/%d/%Y"))

chart.TimeSeries(data)

#intermediate test


#hard test

#base function
chart.TimeSeriesgg <- 
  function(R,
           main = "",
           xaxis = TRUE,
           xlim = NULL,
           xlab = NULL,
           yaxis = TRUE,
           ylim = NULL,
           ylab = NULL,
           gridline = TRUE,
           grid.color = 'lightgray',
           grid.thick = 1,
           date.format.in="%Y-%m-%d", 
           date.format = NULL)
    {
    y = checkData(R,method='xts')
    
    # Set up dimensions and labels
    #legacy code from original chart.TimeSeries function
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    
    if (is.null(date.format)){
      freq = periodicity(y)
      yr_eq <- ifelse(format(index(first(y)),format="%Y")==format(index(last(y)),format="%Y"),TRUE,FALSE) 
      switch(freq$scale,
             seconds = { date.format = "%H:%M"},
             minute = { date.format = "%H:%M"},
             hourly = {date.format = "%d %H"},
             daily = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
             weekly = {if (yr_eq) date.format = "%b %d" else date.format = "%Y-%m-%d"},
             monthly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
             quarterly = {if (yr_eq) date.format = "%b" else date.format = "%b %y"},
             yearly = {date.format = "%Y"}
      )
    }
    # Needed for finding aligned dates for event lines and period areas
    rownames = as.Date(time(y))
    rownames = format(strptime(rownames,format = date.format.in), date.format)
    
    # transform the input data into data frame, so that multi-dimension data can be compressed
    y = data.frame(date=index(data),coredata(data))
    y <- y %>%
      gather(key = "variable", value = "value", -date)

    #pack for delivery
    passon_list = list(y,main,xlim,ylim,xlab,ylab,
                       gridline,grid.color,grid.thick)
    
    invisible(passon_list)

    return(chart.TimeSeriesgg.base(passon_list))
  }

chart.TimeSeriesgg.base <-
  function(passon_list){
    y = passon_list[[1]]
    
    print(class(y))
    main = passon_list[[2]]
    xlim = passon_list[[3]]
    ylim = passon_list[[4]]
    
    xlab = passon_list[[5]]
    ylab = passon_list[[6]]
    gridline = passon_list[[7]]
    grid.color = passon_list[[8]]
    grid.thick = passon_list[[9]]

    plot <- ggplot(y, aes(x = date, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      ggtitle(main)
    
    return(plot)
  }

chart.TimeSeriesgg(data)
